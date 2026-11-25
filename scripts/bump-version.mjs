#!/usr/bin/env node
// Bumps the SDK version across all 40 SDKs: package manifests, source-level version
// constants (and the tests that assert them), and README install pins.
//
//   node scripts/bump-version.mjs <old> <new> [--dry-run] [--dart-note "<text>"]
//
// Every edit is a line-anchored regex with an expected match count. All edits are
// validated before anything is written: if any file doesn't match, the script lists
// every failing edit and exits 1 without changing a file. Files keep their line
// endings and UTF-8 BOM. --dry-run validates and reports every edit without writing.
// --dart-note adds a "## <new>" entry with that text to the top of dart/CHANGELOG.md.
// The Lua rockspec is renamed with `git mv`. Standard library only.
//
// When a new manifest, version constant, or install pin appears, add an edit here and
// a check to scripts/verify-version-sync.sh.

import { execFileSync } from 'node:child_process';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const UTF8_BOM = Buffer.from([0xef, 0xbb, 0xbf]);
const USAGE = 'Usage: node scripts/bump-version.mjs <old> <new> [--dry-run] [--dart-note "<text>"]';

function usage(message) {
    if (message) console.error(`bump-version: ${message}`);
    console.error(USAGE);
    process.exit(2);
}

// ── Arguments ───────────────────────────────────────────────────────────
// Parsed by hand: util.parseArgs rejects option values that start with "-",
// and Dart CHANGELOG notes are "- ..." bullets.
const positional = [];
let dryRun = false;
let dartNote = null;
const argv = process.argv.slice(2);
for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === '--dry-run') {
        dryRun = true;
    } else if (arg === '--dart-note') {
        if (i + 1 >= argv.length) usage('--dart-note needs a value');
        dartNote = argv[++i];
    } else if (arg.startsWith('--dart-note=')) {
        dartNote = arg.slice('--dart-note='.length);
    } else if (arg === '-h' || arg === '--help') {
        console.log(USAGE);
        process.exit(0);
    } else if (arg.startsWith('-')) {
        usage(`unknown option ${arg}`);
    } else {
        positional.push(arg);
    }
}
if (positional.length !== 2) usage('expected an <old> and a <new> version');
const [OLD, NEW] = positional;
for (const v of [OLD, NEW]) {
    if (!/^\d+\.\d+\.\d+$/.test(v)) usage(`"${v}" is not a MAJOR.MINOR.PATCH version`);
}
if (OLD === NEW) usage('<old> and <new> are the same version');
if (dartNote !== null && dartNote.trim() === '') usage('--dart-note text is empty');

// ── Edits ───────────────────────────────────────────────────────────────
const re = String.raw;
const O = OLD.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
const [oldMajor, oldMinor, oldPatch] = OLD.split('.');
const [newMajor, newMinor, newPatch] = NEW.split('.');
const OLD_ROCKSPEC = `lua/copilot-sdk-supercharged-${OLD}-1.rockspec`;
const NEW_ROCKSPEC = `lua/copilot-sdk-supercharged-${NEW}-1.rockspec`;

// Patterns get the g and m flags, so ^ and $ anchor to lines. A string replacement is
// inserted literally; a function receives the match (m[1] is the first group) and the
// file's line ending. Edits to one file apply in order, each to the previous result.
const edit = (file, find, to, count = 1, flags = 'm') => ({ file, find, to, count, flags });

const EDITS = [
    // Manifests, version constants, and the test that asserts one (the v3.1.1 bump).
    edit('ada/alire.toml', re`^version = "${O}"`, `version = "${NEW}"`),
    edit('dart/pubspec.yaml', re`^version: ${O}`, `version: ${NEW}`),
    edit('delphi/boss.json', re`^(\s*)"version": "${O}",`, (m) => `${m[1]}"version": "${NEW}",`),
    edit('dotnet/src/Microsoft.Copilot.SDK.csproj', re`<Version>${O}</Version>`, `<Version>${NEW}</Version>`),
    edit('elixir/mix.exs', re`@version "${O}"`, `@version "${NEW}"`),
    edit('fortran/fpm.toml', re`^version = "${O}"`, `version = "${NEW}"`),
    edit('groovy/build.gradle', re`^version = '${O}'`, `version = '${NEW}'`),
    edit('haskell/copilot-sdk-supercharged.cabal', re`^version:(\s+)${O}\.0`, (m) => `version:${m[1]}${NEW}.0`),
    edit('java/pom.xml', re`^    <version>${O}</version>`, `    <version>${NEW}</version>`),
    edit('julia/Project.toml', re`^version = "${O}"`, `version = "${NEW}"`),
    edit('kotlin/build.gradle.kts', re`^version = "${O}"`, `version = "${NEW}"`),
    edit('matlab/info.xml', re`^  <version>${O}</version>`, `  <version>${NEW}</version>`),
    edit('nim/copilot_sdk.nimble', re`^version(\s+)= "${O}"`, (m) => `version${m[1]}= "${NEW}"`),
    edit('nodejs/package.json', re`^    "version": "${O}",`, `    "version": "${NEW}",`),
    edit('nodejs/package-lock.json',
        re`^(    "name": "copilot-sdk-supercharged",\r?\n    "version": )"${O}"`,
        (m) => `${m[1]}"${NEW}"`),
    edit('nodejs/package-lock.json',
        re`^(        "": \{\r?\n            "name": "copilot-sdk-supercharged",\r?\n            "version": )"${O}"`,
        (m) => `${m[1]}"${NEW}"`),
    edit('python/pyproject.toml', re`^version = "${O}"`, `version = "${NEW}"`),
    edit('r/DESCRIPTION', re`^Version: ${O}`, `Version: ${NEW}`),
    edit('ruby/lib/copilot/version.rb', re`VERSION = "${O}"`, `VERSION = "${NEW}"`),
    edit('rust/Cargo.toml', re`^version = "${O}"`, `version = "${NEW}"`),
    edit('scala/build.sbt', re`version := "${O}",`, `version := "${NEW}",`),
    edit('solidity/package.json', re`^    "version": "${O}",`, `    "version": "${NEW}",`),
    edit('tcl/lib/copilot/pkgIndex.tcl', re`copilot::version ${O} `, `copilot::version ${NEW} `),
    edit('tcl/lib/copilot/version.tcl', re`sdk_version "${O}"`, `sdk_version "${NEW}"`),
    edit('tcl/lib/copilot/version.tcl', re`package provide copilot::version ${O}`, `package provide copilot::version ${NEW}`),
    edit('tcl/tests/client_test.tcl', re`-result "${O}"`, `-result "${NEW}"`, 2),
    edit('visualbasic/CopilotSDK.Supercharged.VB.vbproj', re`<Version>${O}</Version>`, `<Version>${NEW}</Version>`),

    // Lua: the rockspec is read from its old name and written under the new one.
    edit(OLD_ROCKSPEC, re`^version = "${O}-1"`, `version = "${NEW}-1"`),
    edit(OLD_ROCKSPEC, re`tag = "v${O}",`, `tag = "v${NEW}",`),

    // Manifests the v3.1.1 bump missed.
    edit('c/CMakeLists.txt', re`^([ \t]+VERSION )${O}$`, (m) => `${m[1]}${NEW}`),
    edit('clojure/build.clj',
        re`^\(def version \(or \(System/getenv "VERSION"\) "${O}"\)\)$`,
        `(def version (or (System/getenv "VERSION") "${NEW}"))`),
    edit('cpp/CMakeLists.txt',
        re`^project\(copilot-sdk-supercharged VERSION ${O} `,
        `project(copilot-sdk-supercharged VERSION ${NEW} `),
    edit('crystal/shard.yml', re`^version: ${O}$`, `version: ${NEW}`),
    edit('dlang/dub.json', re`^    "version": "${O}",$`, `    "version": "${NEW}",`),
    edit('erlang/src/copilot_sdk.app.src', re`^([ \t]*\{vsn, )"${O}"\},$`, (m) => `${m[1]}"${NEW}"},`),
    edit('fortran/CMakeLists.txt', re`^([ \t]+VERSION )${O}$`, (m) => `${m[1]}${NEW}`),
    edit('fsharp/src/CopilotSDK.Supercharged.FSharp.fsproj', re`<Version>${O}</Version>`, `<Version>${NEW}</Version>`),
    edit('objc/CopilotSDK.podspec', re`^([ \t]*s\.version[ \t]*= )"${O}"$`, (m) => `${m[1]}"${NEW}"`),
    edit('ocaml/copilot-sdk-supercharged.opam', re`^version: "${O}"$`, `version: "${NEW}"`),
    edit('ocaml/dune-project', re`^\(version ${O}\)$`, `(version ${NEW})`),
    edit('perl/lib/GitHub/Copilot/Client.pm', re`^our \$VERSION = '${O}';$`, `our $VERSION = '${NEW}';`),
    edit('vlang/v.mod', re`^([ \t]+version: )'${O}'$`, (m) => `${m[1]}'${NEW}'`),
    edit('zig/build.zig.zon', re`^([ \t]*\.version = )"${O}",$`, (m) => `${m[1]}"${NEW}",`),

    // Source-level SDK version constants the v3.1.1 bump missed, and the tests that assert them.
    edit('ada/src/copilot-version.ads',
        re`^([ \t]*SDK_Version[ \t]*: constant String := )"${O}";$`,
        (m) => `${m[1]}"${NEW}";`),
    edit('ada/tests/test_client.adb', re`Copilot\.Version\.SDK_Version = "${O}",`, `Copilot.Version.SDK_Version = "${NEW}",`),
    edit('ada/tests/test_client.adb', re`"SDK_Version is ${O}"`, `"SDK_Version is ${NEW}"`),
    edit('c/include/copilot/copilot.h', re`^#define COPILOT_SDK_VERSION_MAJOR ${oldMajor}$`, `#define COPILOT_SDK_VERSION_MAJOR ${newMajor}`),
    edit('c/include/copilot/copilot.h', re`^#define COPILOT_SDK_VERSION_MINOR ${oldMinor}$`, `#define COPILOT_SDK_VERSION_MINOR ${newMinor}`),
    edit('c/include/copilot/copilot.h', re`^#define COPILOT_SDK_VERSION_PATCH ${oldPatch}$`, `#define COPILOT_SDK_VERSION_PATCH ${newPatch}`),
    edit('cobol/src/COPILOT-VERSION.cpy',
        re`^([ \t]*05 WS-SDK-VERSION[ \t]+PIC X\(10\)[ \t]+VALUE )"${O}"\.$`,
        (m) => `${m[1]}"${NEW}".`),
    edit('cobol/tests/TEST-CLIENT.cob', re`AND WS-SDK-VERSION = "${O}"$`, `AND WS-SDK-VERSION = "${NEW}"`),
    edit('cobol/tests/TEST-CLIENT.cob', re`copilot-sdk-cobol, ${O}"`, `copilot-sdk-cobol, ${NEW}"`),
    edit('crystal/src/copilot_sdk/version.cr', re`^([ \t]*VERSION = )"${O}"$`, (m) => `${m[1]}"${NEW}"`),
    edit('ocaml/lib/version.ml', re`^let sdk_version = "${O}"$`, `let sdk_version = "${NEW}"`),
    edit('shell/lib/copilot_sdk.sh', re`^COPILOT_SDK_VERSION="${O}"$`, `COPILOT_SDK_VERSION="${NEW}"`),
    edit('vlang/src/version.v', re`^pub const sdk_version = '${O}'$`, `pub const sdk_version = '${NEW}'`),
    edit('vlang/src/client_test.v', re`^([ \t]*assert sdk_version == )'${O}'$`, (m) => `${m[1]}'${NEW}'`),

    // Install pins that must match the published (or locally built) version.
    edit('java/README.md', re`<version>${O}</version>`, `<version>${NEW}</version>`),
    edit('java/README.md', re`copilot-sdk-supercharged:${O}\b`, `copilot-sdk-supercharged:${NEW}`, 2),
    edit('kotlin/README.md', re`copilot-sdk-supercharged-kotlin:${O}\b`, `copilot-sdk-supercharged-kotlin:${NEW}`),
    edit('groovy/README.md', re`copilot-sdk-groovy:${O}\b`, `copilot-sdk-groovy:${NEW}`),
    edit('scala/README.md', re`% "${O}"`, `% "${NEW}"`),
    edit('clojure/README.md', re`\{:mvn/version "${O}"\}`, `{:mvn/version "${NEW}"}`),
    edit('clojure/README.md', re`copilot-sdk-supercharged "${O}"\]`, `copilot-sdk-supercharged "${NEW}"]`),
    edit('julia/README.md', re`rev="v${O}"`, `rev="v${NEW}"`),
    edit('lua/README.md', re`copilot-sdk-supercharged-${O}-1\.rockspec`, `copilot-sdk-supercharged-${NEW}-1.rockspec`),
    edit('erlang/README.md', re`--branch v${O}\b`, `--branch v${NEW}`),
    edit('java/e2e/pom.xml',
        re`^([ \t]*<artifactId>copilot-sdk-supercharged</artifactId>\r?\n[ \t]*<version>)${O}(</version>)$`,
        (m) => `${m[1]}${NEW}${m[2]}`),
    edit('java/e2e/pom.xml', re`copilot-sdk-supercharged-${O}\.jar`, `copilot-sdk-supercharged-${NEW}.jar`),
];

if (dartNote !== null) {
    // No m flag: ^ is the start of the file, which must begin with the "## <old>" entry.
    EDITS.push(edit('dart/CHANGELOG.md', re`^## ${O}`,
        (m, eol) => `## ${NEW}${eol}${dartNote.trimEnd().split(/\r\n|\r|\n/).join(eol)}${eol}## ${OLD}`, 1, ''));
}

// ── Validate every edit before writing anything ─────────────────────────
const files = new Map();

function load(file) {
    if (!files.has(file)) {
        const path = join(ROOT, file);
        let entry;
        if (!existsSync(path)) {
            entry = { error: 'file not found' };
        } else {
            const bytes = readFileSync(path);
            const bom = bytes.length >= 3 && bytes[0] === 0xef && bytes[1] === 0xbb && bytes[2] === 0xbf;
            const body = bom ? bytes.subarray(3) : bytes;
            const text = body.toString('utf8');
            // A lossy decode would corrupt the file on write.
            entry = Buffer.from(text, 'utf8').equals(body)
                ? { bom, text, original: text }
                : { error: 'not valid UTF-8, refusing to rewrite it' };
        }
        files.set(file, entry);
    }
    return files.get(file);
}

// One report entry per check, in order: the rockspec rename, then every edit.
const report = [];
const fail = (text) => report.push({ ok: false, text: `FAIL ${text}` });

if (existsSync(join(ROOT, NEW_ROCKSPEC))) {
    fail(`${NEW_ROCKSPEC}: already exists, so ${OLD_ROCKSPEC} can't be renamed to it`);
} else if (existsSync(join(ROOT, OLD_ROCKSPEC))) {
    try {
        execFileSync('git', ['ls-files', '--error-unmatch', '--', OLD_ROCKSPEC], { cwd: ROOT, stdio: 'pipe' });
        report.push({ ok: true, text: `git mv ${OLD_ROCKSPEC} ${NEW_ROCKSPEC}` });
    } catch (err) {
        fail(err.code === 'ENOENT'
            ? `git not found; it's needed to rename ${OLD_ROCKSPEC}`
            : `${OLD_ROCKSPEC}: not tracked by git, so it can't be renamed with git mv`);
    }
}

for (const e of EDITS) {
    const file = load(e.file);
    if (file.error) {
        fail(`${e.file}: ${file.error}`);
        continue;
    }
    const rx = new RegExp(e.find, `g${e.flags}`);
    const found = [...file.text.matchAll(rx)].length;
    if (found !== e.count) {
        fail(`${e.file}: expected ${e.count} match(es) for /${e.find}/, found ${found}`);
        continue;
    }
    const eol = file.text.includes('\r\n') ? '\r\n' : '\n';
    file.text = file.text.replace(rx, (...m) => (typeof e.to === 'function' ? e.to(m, eol) : e.to));
    report.push({ ok: true, file: e.file, found, text: `${e.file} (${found})` });
}

const problems = report.filter((r) => !r.ok);
const fileCount = new Set(EDITS.map((e) => e.file)).size;
const checked = `${EDITS.length} edits across ${fileCount} files`;
const dartSkip = dartNote === null
    ? `skip dart/CHANGELOG.md (no --dart-note; pub.dev expects a CHANGELOG entry for ${NEW})`
    : null;
console.log(`bump-version ${OLD} -> ${NEW}${dryRun ? ' (dry run)' : ''}`);

if (dryRun) {
    for (const r of report) console.log(r.ok ? `ok (dry) ${r.text}` : r.text);
    if (dartSkip) console.log(dartSkip);
    if (problems.length > 0) {
        console.log(`Dry run failed: ${problems.length} problem(s) in ${checked}. Nothing was written.`);
        process.exit(1);
    }
    console.log(`Dry run passed: all ${checked} match. Nothing was written.`);
    process.exit(0);
}

if (problems.length > 0) {
    for (const r of problems) console.error(r.text);
    console.error(`Aborted: ${problems.length} problem(s) in ${checked}. Nothing was written. Run with --dry-run for the full report.`);
    process.exit(1);
}

// ── Apply ───────────────────────────────────────────────────────────────
// Rename first: if git mv fails, no file has been written yet.
try {
    execFileSync('git', ['mv', '--', OLD_ROCKSPEC, NEW_ROCKSPEC], { cwd: ROOT, stdio: ['ignore', 'pipe', 'pipe'] });
} catch (err) {
    console.error(`FAIL git mv ${OLD_ROCKSPEC} ${NEW_ROCKSPEC}: ${String(err.stderr || err.message).trim()}`);
    console.error('Nothing was written.');
    process.exit(1);
}
console.log(`renamed ${OLD_ROCKSPEC} -> ${NEW_ROCKSPEC} (git mv)`);

const written = [];
for (const [file, entry] of files) {
    if (entry.text === entry.original) continue;
    const target = file === OLD_ROCKSPEC ? NEW_ROCKSPEC : file;
    const body = Buffer.from(entry.text, 'utf8');
    try {
        writeFileSync(join(ROOT, target), entry.bom ? Buffer.concat([UTF8_BOM, body]) : body);
    } catch (err) {
        console.error(`FAIL writing ${target}: ${err.message}`);
        console.error(`Already renamed the rockspec and wrote ${written.length} file(s)${written.length ? `: ${written.join(', ')}` : ''}. Undo with git mv and git restore.`);
        process.exit(1);
    }
    written.push(target);
}
for (const r of report) {
    if (r.file) console.log(`updated ${r.file === OLD_ROCKSPEC ? NEW_ROCKSPEC : r.file} (${r.found})`);
}
if (dartSkip) console.log(dartSkip);
console.log(`Bumped ${OLD} -> ${NEW}: ${EDITS.length} edits across ${written.length} files.`);
console.log('Next: bash scripts/verify-version-sync.sh, then add the CHANGELOG.md and README.md release notes.');
