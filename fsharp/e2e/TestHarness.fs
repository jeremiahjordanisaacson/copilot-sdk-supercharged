module CopilotSDK.Supercharged.FSharp.E2E.TestHarness

open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions

/// Locates the Copilot CLI used by E2E tests.
///
/// Lookup order:
/// 1. COPILOT_CLI_PATH, when set and the file exists (CI points it at
///    nodejs/node_modules/.bin/copilot).
/// 2. nodejs/node_modules/@github/copilot/npm-loader.js (CLI 1.0.113+).
/// 3. Legacy nodejs/node_modules/@github/copilot-*/index.js and
///    nodejs/node_modules/@github/copilot/index.js.
module CliDiscovery =
    /// Absolute path of the repository root.
    let repoRoot () =
        IO.Path.GetFullPath(IO.Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))

    /// Find the CLI entry point, or None when no CLI is installed.
    let findCliPath () : string option =
        let fromEnv =
            match Environment.GetEnvironmentVariable("COPILOT_CLI_PATH") with
            | null | "" -> None
            | path when IO.File.Exists(path) -> Some path
            | _ -> None
        let scope = IO.Path.Combine(repoRoot (), "nodejs", "node_modules", "@github")
        let existing (path: string) = if IO.File.Exists(path) then Some path else None
        let legacyPlatformIndex () =
            if IO.Directory.Exists(scope) then
                IO.Directory.GetDirectories(scope, "copilot-*")
                |> Array.map (fun dir -> IO.Path.Combine(dir, "index.js"))
                |> Array.tryFind IO.File.Exists
            else None
        fromEnv
        |> Option.orElse (existing (IO.Path.Combine(scope, "copilot", "npm-loader.js")))
        |> Option.orElseWith legacyPlatformIndex
        |> Option.orElse (existing (IO.Path.Combine(scope, "copilot", "index.js")))

    /// Executable and leading arguments that run the CLI at cliPath: `.js`
    /// entry points run with node, anything else (such as the .bin/copilot
    /// shim, which has a node shebang) runs directly.
    let commandFor (cliPath: string) : string * string list =
        if cliPath.EndsWith(".js", StringComparison.OrdinalIgnoreCase) then
            "node", [ cliPath ]
        else
            cliPath, []

/// Manages the replaying CAPI proxy process for E2E tests.
/// Spawns `npx tsx ../../test/harness/server.ts` and parses the listening URL.
type ReplayProxy() =
    let mutable proc: Process option = None
    let mutable proxyUrl: string option = None

    /// Start the replay proxy and return its URL (e.g. "http://localhost:PORT").
    member _.StartProxy() : string =
        match proxyUrl with
        | Some url -> url
        | None ->

        let serverPath =
            IO.Path.GetFullPath(
                IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "test", "harness", "server.ts"))

        let workingDir = IO.Path.GetDirectoryName(serverPath)
        let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

        let psi = ProcessStartInfo()
        if isWindows then
            psi.FileName <- "cmd"
            psi.Arguments <- sprintf "/c npx tsx \"%s\"" serverPath
        else
            psi.FileName <- "npx"
            psi.Arguments <- sprintf "tsx \"%s\"" serverPath

        psi.WorkingDirectory <- workingDir
        psi.UseShellExecute <- false
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- false
        psi.CreateNoWindow <- true

        let p = Process.Start(psi)
        proc <- Some p

        // Read first line to get listening URL
        let line = p.StandardOutput.ReadLine()
        if String.IsNullOrEmpty(line) then
            p.Kill()
            failwith "Failed to read proxy URL from stdout"

        let m = Regex.Match(line, @"Listening: (http://[^\s]+)")
        if not m.Success then
            p.Kill()
            failwithf "Unexpected proxy output: %s" line

        let url = m.Groups.[1].Value
        proxyUrl <- Some url
        url

    /// Stop the replay proxy process.
    member _.StopProxy() =
        proc |> Option.iter (fun p ->
            try
                if not p.HasExited then p.Kill()
                p.WaitForExit()
                p.Dispose()
            with _ -> ())
        proc <- None
        proxyUrl <- None

    /// The proxy URL, or None if not started.
    member _.Url = proxyUrl

    interface IDisposable with
        member this.Dispose() = this.StopProxy()
