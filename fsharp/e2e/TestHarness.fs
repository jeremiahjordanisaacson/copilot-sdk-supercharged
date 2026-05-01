module CopilotSDK.Supercharged.FSharp.E2E.TestHarness

open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text.RegularExpressions

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
