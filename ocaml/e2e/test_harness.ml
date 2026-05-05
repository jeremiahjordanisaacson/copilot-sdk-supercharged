(* E2E test harness for the OCaml SDK.
   Spawns the shared replay proxy, parses the Listening URL,
   and provides helpers for E2E tests. *)

let server_path =
  let dir = Filename.dirname (Filename.dirname Sys.argv.(0)) in
  Filename.concat (Filename.concat (Filename.concat dir "..") "test")
    (Filename.concat "harness" "server.ts")

type proxy = {
  pid : int;
  url : string;
}

let start_proxy () =
  let abs_server =
    (* Navigate from ocaml/ (cwd) to repo root test/harness/server.ts *)
    let repo_root = Filename.dirname (Sys.getcwd ()) in
    Filename.concat repo_root
      (Filename.concat "test" (Filename.concat "harness" "server.ts"))
  in
  let cwd = Filename.dirname abs_server in
  let cmd = Printf.sprintf "cd %s && npx tsx %s" (Filename.quote cwd) (Filename.quote abs_server) in
  let ic = Unix.open_process_in cmd in
  let line = input_line ic in
  let url =
    let re = Str.regexp {|Listening: \(http://[^ ]+\)|} in
    if Str.string_match re line 0 then Str.matched_group 1 line
    else failwith (Printf.sprintf "Could not parse proxy URL from: %s" line)
  in
  (* We need to keep the channel alive so the process stays running *)
  let pid = Unix.getpid () in  (* placeholder -- proxy runs in background *)
  ignore pid;
  Printf.printf "[ocaml-e2e] Proxy listening at %s\n%!" url;
  Unix.putenv "COPILOT_API_URL" url;
  { pid = 0; url }

let stop_proxy proxy =
  ignore proxy;
  (* The proxy process gets cleaned up when the pipe closes *)
  ()

let proxy_url proxy = proxy.url
