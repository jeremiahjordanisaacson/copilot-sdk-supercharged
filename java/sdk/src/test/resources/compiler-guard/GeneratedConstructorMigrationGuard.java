import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.MemberReferenceTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.NewClassTree;
import com.sun.source.tree.Tree;
import com.sun.source.util.JavacTask;
import com.sun.source.util.TreePath;
import com.sun.source.util.TreePathScanner;
import com.sun.source.util.Trees;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.ArrayType;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;

public final class GeneratedConstructorMigrationGuard {
    private static final String MODEL_SWITCH_DESCRIPTOR =
            "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;"
                    + "Lcom/microsoft/copilot/generated/rpc/AutoTier;Lcom/microsoft/copilot/generated/rpc/FusionTier;Ljava/lang/String;"
                    + "Lcom/microsoft/copilot/generated/rpc/ReasoningSummary;Lcom/microsoft/copilot/generated/rpc/Verbosity;"
                    + "Lcom/microsoft/copilot/generated/rpc/ModelCapabilitiesOverride;"
                    + "Lcom/microsoft/copilot/generated/rpc/ContextTier;"
                    + "Lcom/microsoft/copilot/generated/rpc/ModelChangeSource;Ljava/lang/Boolean;Ljava/lang/String;"
                    + "Ljava/lang/Boolean;Ljava/lang/String;Ljava/lang/String;Ljava/lang/Boolean;"
                    + "Lcom/microsoft/copilot/generated/rpc/ModelPickerPersistenceRequest;Ljava/util/Set;)V";
    private static final List<CompatibilityConstruction> APPROVED = List.of(
            new CompatibilityConstruction(
                    new Construction(
                            "src/main/java/com/microsoft/copilot/CopilotSession.java",
                            "com.microsoft.copilot.CopilotSession#setModel(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;"
                                    + "Lcom/microsoft/copilot/rpc/ModelCapabilitiesOverride;)"
                                    + "Ljava/util/concurrent/CompletableFuture;",
                            "com.microsoft.copilot.generated.rpc.SessionModelSwitchToParams",
                            MODEL_SWITCH_DESCRIPTOR,
                            "new"),
                    "Canonical construction preserves null-model omission in the existing setModel overload."),
            new CompatibilityConstruction(
                    new Construction(
                            "src/test/java/com/microsoft/copilot/SessionEventDeserializationTest.java",
                            "com.microsoft.copilot.SessionEventDeserializationTest"
                                    + "#testToolStartCanonicalConstructorAcceptsNullOptionalArguments()V",
                            "com.microsoft.copilot.generated.ToolExecutionStartEvent$ToolExecutionStartEventData",
                            "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/Boolean;"
                                    + "Ljava/lang/Object;Lcom/microsoft/copilot/generated/ToolExecutionStartShellToolInfo;"
                                    + "Ljava/lang/String;Ljava/lang/Boolean;Ljava/lang/String;Ljava/lang/String;"
                                    + "Ljava/lang/String;Lcom/microsoft/copilot/generated/McpServerTransport;"
                                    + "Lcom/microsoft/copilot/generated/McpServerSource;Ljava/lang/String;Ljava/lang/Boolean;"
                                    + "Lcom/microsoft/copilot/generated/ToolExecutionStartToolDescription;"
                                    + "Lcom/microsoft/copilot/generated/FusionAttribution;Ljava/lang/String;Ljava/util/Set;)V",
                            "new"),
                    "Canonical serialization probe verifies normalization of null optional arguments and presence set."));

    private record Construction(String source, String enclosing, String target, String descriptor, String kind) {}

    private record CompatibilityConstruction(Construction construction, String protectedBehavior) {}

    private record Inspection(Set<String> selectedTypes, List<Construction> constructions, List<String> unresolved,
            List<String> errors) {}

    public static void main(String[] args) throws Exception {
        Path moduleRoot = Path.of(args[0]).toAbsolutePath().normalize();
        Path generatedRoot = moduleRoot.resolve("src/generated/java");
        Path productionRoot = moduleRoot.resolve("src/main/java");
        Path testRoot = moduleRoot.resolve("src/test/java");
        List<Path> generated = javaSources(generatedRoot);
        List<Path> production = javaSources(productionRoot);
        List<Path> tests = javaSources(testRoot);
        require(!generated.isEmpty(), "Generated source scope must be nonempty");
        require(!production.isEmpty(), "Production source scope must be nonempty");
        require(!tests.isEmpty(), "Test source scope must be nonempty");

        List<Path> sources = new ArrayList<>(generated);
        sources.addAll(production);
        sources.addAll(tests);
        Inspection inspection = inspect(moduleRoot, generatedRoot, productionRoot, testRoot, sources, args[1]);
        require(inspection.errors().isEmpty(),
                "Compiler errors prevent attribution:\n" + String.join("\n", inspection.errors()));
        require(!inspection.selectedTypes().isEmpty(), "Generated DTO ownership scope must be nonempty");
        require(inspection.unresolved().isEmpty(),
                "Unresolved constructor sites:\n" + String.join("\n", inspection.unresolved()));

        Set<Construction> exceptionKeys = new HashSet<>();
        for (CompatibilityConstruction exception : APPROVED) {
            exceptionKeys.add(exception.construction());
            require(!exception.protectedBehavior().isBlank(),
                    "Compatibility exception must name the protected behavior: " + exception.construction());
        }
        require(exceptionKeys.size() == APPROVED.size(), "Duplicate compatibility exceptions");
        Map<Construction, Integer> exceptionCounts = new HashMap<>();
        for (CompatibilityConstruction exception : APPROVED) {
            exceptionCounts.put(exception.construction(), 0);
        }
        List<String> violations = new ArrayList<>();
        for (Construction site : inspection.constructions()) {
            Construction key = new Construction(site.source(), site.enclosing(), site.target(), site.descriptor(),
                    site.kind());
            if (exceptionKeys.contains(key)) {
                exceptionCounts.compute(key, (ignored, count) -> count + 1);
            } else {
                violations.add(site.source() + ":" + site.enclosing() + " constructs " + site.target()
                        + site.descriptor() + " using " + site.kind());
            }
        }
        for (CompatibilityConstruction exception : APPROVED) {
            int count = exceptionCounts.get(exception.construction());
            require(count == 1, "Stale or multiplied compatibility exception (" + exception.protectedBehavior()
                    + "): " + exception.construction());
        }
        require(violations.isEmpty(),
                "Unapproved generated DTO constructor sites:\n" + String.join("\n", violations));
        System.out.println("Resolved " + inspection.selectedTypes().size() + " generated DTO identities; "
                + inspection.constructions().size() + " positional sites match " + APPROVED.size()
                + " exact compatibility exceptions.");
    }

    private static List<Path> javaSources(Path root) throws Exception {
        require(Files.isDirectory(root), "Missing source scope: " + root);
        try (var paths = Files.walk(root)) {
            return paths.filter(path -> Files.isRegularFile(path) && path.toString().endsWith(".java"))
                    .filter(path -> !path.getFileName().toString().equals("module-info.java"))
                    .sorted(Comparator.comparing(Path::toString)).toList();
        }
    }

    private static Inspection inspect(Path moduleRoot, Path generatedRoot, Path productionRoot, Path testRoot,
            List<Path> sources, String classpath) throws Exception {
        var compiler = ToolProvider.getSystemJavaCompiler();
        require(compiler != null, "A full JDK is required for resolved constructor attribution");
        var diagnostics = new DiagnosticCollector<JavaFileObject>();
        try (var manager = compiler.getStandardFileManager(diagnostics, null, null)) {
            var options = List.of("--release", "17", "-encoding", "UTF-8", "-proc:none", "-classpath", classpath,
                    "-implicit:none");
            var task = (JavacTask) compiler.getTask(null, manager, diagnostics, options, null,
                    manager.getJavaFileObjectsFromPaths(sources));
            List<CompilationUnitTree> units = new ArrayList<>();
            task.parse().forEach(units::add);
            task.analyze();

            Trees trees = Trees.instance(task);
            Types types = task.getTypes();
            Elements elements = task.getElements();
            TypeElement sessionEvent = elements.getTypeElement("com.microsoft.copilot.generated.SessionEvent");
            Set<String> selectedTypes = new HashSet<>();
            for (CompilationUnitTree unit : units) {
                Path source = Path.of(unit.getSourceFile().toUri()).toAbsolutePath().normalize();
                if (!source.startsWith(generatedRoot)) {
                    continue;
                }
                new TreePathScanner<Void, Void>() {
                    @Override
                    public Void visitClass(ClassTree tree, Void ignored) {
                        Element element = trees.getElement(getCurrentPath());
                        if (element instanceof TypeElement type && isSelected(type, sessionEvent, types)) {
                            selectedTypes.add(elements.getBinaryName(type).toString());
                        }
                        return super.visitClass(tree, ignored);
                    }
                }.scan(unit, null);
            }

            List<Construction> constructions = new ArrayList<>();
            List<String> unresolved = new ArrayList<>();
            for (CompilationUnitTree unit : units) {
                Path source = Path.of(unit.getSourceFile().toUri()).toAbsolutePath().normalize();
                if (!source.startsWith(productionRoot) && !source.startsWith(testRoot)) {
                    continue;
                }
                String logicalSource = moduleRoot.relativize(source).toString().replace(File.separatorChar, '/');
                new TreePathScanner<Void, Void>() {
                    private String enclosing() {
                        for (TreePath cursor = getCurrentPath(); cursor != null; cursor = cursor.getParentPath()) {
                            if (cursor.getLeaf() instanceof MethodTree) {
                                Element element = trees.getElement(cursor);
                                if (element instanceof ExecutableElement executable
                                        && executable.getEnclosingElement() instanceof TypeElement owner) {
                                    return elements.getBinaryName(owner) + "#" + executable.getSimpleName()
                                            + descriptor(executable, types, elements);
                                }
                            }
                        }
                        return "<initializer>";
                    }

                    private void record(Tree tree, String kind) {
                        Element element = trees.getElement(getCurrentPath());
                        if (!(element instanceof ExecutableElement executable)
                                || !(executable.getEnclosingElement() instanceof TypeElement owner)) {
                            if (tree instanceof MemberReferenceTree reference
                                    && reference.getQualifierExpression().getKind() == Tree.Kind.ARRAY_TYPE) {
                                return;
                            }
                            unresolved.add(logicalSource + " unresolved " + kind + ": " + tree);
                            return;
                        }
                        String target = elements.getBinaryName(owner).toString();
                        if (selectedTypes.contains(target)) {
                            constructions.add(new Construction(logicalSource, enclosing(), target,
                                    descriptor(executable, types, elements), kind));
                        }
                    }

                    @Override
                    public Void visitNewClass(NewClassTree tree, Void ignored) {
                        record(tree, "new");
                        return super.visitNewClass(tree, ignored);
                    }

                    @Override
                    public Void visitMemberReference(MemberReferenceTree tree, Void ignored) {
                        if (tree.getMode() == MemberReferenceTree.ReferenceMode.NEW) {
                            record(tree, "constructor-reference");
                        }
                        return super.visitMemberReference(tree, ignored);
                    }

                    @Override
                    public Void visitMethodInvocation(MethodInvocationTree tree, Void ignored) {
                        Element element = trees.getElement(getCurrentPath());
                        if (element instanceof ExecutableElement executable
                                && executable.getKind() == ElementKind.CONSTRUCTOR) {
                            record(tree, "constructor-delegation");
                        }
                        return super.visitMethodInvocation(tree, ignored);
                    }
                }.scan(unit, null);
            }

            List<String> errors = diagnostics.getDiagnostics().stream()
                    .filter(diagnostic -> diagnostic.getKind() == Diagnostic.Kind.ERROR)
                    .map(GeneratedConstructorMigrationGuard::formatDiagnostic).toList();
            return new Inspection(selectedTypes, constructions, unresolved, errors);
        }
    }

    private static boolean isSelected(TypeElement type, TypeElement sessionEvent, Types types) {
        if (hasJsonValue(type)) {
            return false;
        }
        if (type.getKind() == ElementKind.RECORD) {
            return true;
        }
        if (type.getKind() != ElementKind.CLASS || type.getModifiers().contains(Modifier.ABSTRACT)
                || sessionEvent != null && types.isSubtype(type.asType(), sessionEvent.asType())) {
            return false;
        }
        TypeMirror superclass = type.getSuperclass();
        if (!(superclass instanceof DeclaredType declared)) {
            return false;
        }
        TypeElement parent = (TypeElement) declared.asElement();
        return hasAnnotation(parent, "com.fasterxml.jackson.annotation.JsonTypeInfo");
    }

    private static boolean hasAnnotation(TypeElement type, String annotationName) {
        return type.getAnnotationMirrors().stream()
                .anyMatch(annotation -> annotation.getAnnotationType().toString().equals(annotationName));
    }

    private static boolean hasJsonValue(TypeElement type) {
        return type.getEnclosedElements().stream().filter(element -> element instanceof ExecutableElement)
                .map(element -> (ExecutableElement) element)
                .anyMatch(method -> method.getAnnotationMirrors().stream().anyMatch(annotation -> annotation
                        .getAnnotationType().toString().equals("com.fasterxml.jackson.annotation.JsonValue")));
    }

    private static String descriptor(ExecutableElement executable, Types types, Elements elements) {
        String parameters = executable.getParameters().stream()
                .map(parameter -> descriptor(types.erasure(parameter.asType()), types, elements))
                .reduce("", String::concat);
        String result = executable.getKind() == ElementKind.CONSTRUCTOR ? "V"
                : descriptor(types.erasure(executable.getReturnType()), types, elements);
        return "(" + parameters + ")" + result;
    }

    private static String descriptor(TypeMirror type, Types types, Elements elements) {
        TypeMirror erased = types.erasure(type);
        return switch (erased.getKind()) {
            case BOOLEAN -> "Z";
            case BYTE -> "B";
            case SHORT -> "S";
            case INT -> "I";
            case LONG -> "J";
            case CHAR -> "C";
            case FLOAT -> "F";
            case DOUBLE -> "D";
            case VOID -> "V";
            case ARRAY -> "[" + descriptor(((ArrayType) erased).getComponentType(), types, elements);
            case DECLARED -> "L" + elements.getBinaryName((TypeElement) ((DeclaredType) erased).asElement())
                    .toString().replace('.', '/') + ";";
            default -> throw new IllegalArgumentException("Unresolved descriptor type: " + erased);
        };
    }

    private static String formatDiagnostic(Diagnostic<? extends JavaFileObject> diagnostic) {
        return (diagnostic.getSource() == null ? "" : diagnostic.getSource().getName()) + ":"
                + diagnostic.getLineNumber() + ": " + diagnostic.getMessage(null);
    }

    private static void require(boolean condition, String message) {
        if (!condition) {
            throw new IllegalStateException(message);
        }
    }
}
