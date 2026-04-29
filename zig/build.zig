const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // -----------------------------------------------------------------------
    // Library module (importable as "copilot-sdk")
    // -----------------------------------------------------------------------

    const lib_mod = b.addModule("copilot-sdk", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Static library artifact (for linking from C/C++ or other languages)
    const lib = b.addStaticLibrary(.{
        .name = "copilot-sdk",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);

    // -----------------------------------------------------------------------
    // Unit tests
    // -----------------------------------------------------------------------

    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    // -----------------------------------------------------------------------
    // Example
    // -----------------------------------------------------------------------

    const example = b.addExecutable(.{
        .name = "basic_example",
        .root_source_file = b.path("examples/basic_example.zig"),
        .target = target,
        .optimize = optimize,
    });
    example.root_module.addImport("copilot-sdk", lib_mod);
    b.installArtifact(example);

    const run_example = b.addRunArtifact(example);
    const example_step = b.step("run-example", "Run the basic example");
    example_step.dependOn(&run_example.step);
}
