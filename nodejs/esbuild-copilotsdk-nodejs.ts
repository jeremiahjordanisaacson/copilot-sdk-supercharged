import * as esbuild from "esbuild";
import { execSync } from "child_process";
import { readdirSync, writeFileSync } from "fs";
import { join } from "path";

function listTypeScriptFiles(dir: string): string[] {
    return readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
        const path = join(dir, entry.name);
        if (entry.isDirectory()) return listTypeScriptFiles(path);
        return entry.name.endsWith(".ts") ? [path] : [];
    });
}

const entryPoints = listTypeScriptFiles("src");

// ESM build
await esbuild.build({
    entryPoints,
    outbase: "src",
    outdir: "dist",
    format: "esm",
    platform: "node",
    target: "es2022",
    sourcemap: false,
    outExtension: { ".js": ".js" },
});

// CJS build — uses .js extension with a "type":"commonjs" package.json marker
await esbuild.build({
    entryPoints,
    outbase: "src",
    outdir: "dist/cjs",
    format: "cjs",
    platform: "node",
    target: "es2022",
    sourcemap: false,
    outExtension: { ".js": ".js" },
    logOverride: { "empty-import-meta": "silent" },
});

// Mark the CJS directory so Node treats .js files as CommonJS
writeFileSync("dist/cjs/package.json", JSON.stringify({ type: "commonjs" }) + "\n");

// Generate .d.ts files
execSync("tsc", { stdio: "inherit" });
