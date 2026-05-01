namespace CopilotSDK.Supercharged.FSharp.E2E

open System
open Xunit
open FsUnit.Xunit
open CopilotSDK.Supercharged.FSharp
open CopilotSDK.Supercharged.FSharp.E2E.TestHarness

/// Shared fixture that starts the replay proxy once for all tests in the class.
type ProxyFixture() =
    let proxy = new ReplayProxy()
    let mutable url: string = ""

    do
        url <- proxy.StartProxy()

    /// Full proxy URL, e.g. "http://localhost:12345"
    member _.ProxyUrl = url

    /// CliUrl value (host:port without scheme) for CopilotClientOptions.
    member _.CliUrl =
        let uri = Uri(url)
        sprintf "%s:%d" uri.Host uri.Port

    interface IDisposable with
        member _.Dispose() = proxy.StopProxy()

/// E2E tests for the F# Copilot SDK using the shared replay proxy.
type E2ETests(fixture: ProxyFixture) =

    interface IClassFixture<ProxyFixture>

    let makeClient () =
        let opts =
            { CopilotClientOptions.defaults with
                CliUrl = Some fixture.CliUrl }
        new CopilotClient(opts)

    [<Fact>]
    member _.``Session create and disconnect`` () =
        async {
            use client = makeClient ()
            do! client.StartAsync()

            client.ConnectionState |> should equal Connected

            let! session =
                client.CreateSessionAsync(SessionConfig.defaults)

            session.SessionId |> should not' (equal "")
            session.SessionId |> should not' (be Null)

            do! client.StopAsync()

            client.ConnectionState |> should equal Disconnected
        }
        |> Async.RunSynchronously

    [<Fact>]
    member _.``Send message and receive response`` () =
        async {
            use client = makeClient ()
            do! client.StartAsync()

            let! session =
                client.CreateSessionAsync(SessionConfig.defaults)

            let! reply =
                session.SendAndWaitAsync(
                    { Prompt = "Hello, Copilot!"
                      Attachments = None
                      Mode = None },
                    timeout = TimeSpan.FromSeconds(30.0))

            reply |> should not' (equal None)
            let data = reply |> Option.get
            data.Content |> should not' (be EmptyString)

            do! client.StopAsync()
        }
        |> Async.RunSynchronously

    [<Fact>]
    member _.``SessionFs provider configuration`` () =
        async {
            let fsConfig =
                { InitialCwd = "/workspace"
                  SessionStatePath = "/session-state"
                  Conventions = "posix" }

            let opts =
                { CopilotClientOptions.defaults with
                    CliUrl = Some fixture.CliUrl
                    SessionFs = Some fsConfig }

            use client = new CopilotClient(opts)
            do! client.StartAsync()

            client.ConnectionState |> should equal Connected

            let! session =
                client.CreateSessionAsync(SessionConfig.defaults)

            session.SessionId |> should not' (equal "")

            do! client.StopAsync()
        }
        |> Async.RunSynchronously
