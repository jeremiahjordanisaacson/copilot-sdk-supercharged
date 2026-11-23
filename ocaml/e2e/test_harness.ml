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

(* Locates the Copilot CLI for E2E runs. Lookup order:
   1. COPILOT_CLI_PATH, when set and the file exists (CI points it at
      nodejs/node_modules/.bin/copilot, which runs directly);
   2. nodejs/node_modules/@github/copilot/npm-loader.js (CLI 1.0.113+), run with node;
   3. legacy nodejs/node_modules/@github/copilot-*/index.js and
      nodejs/node_modules/@github/copilot/index.js. *)
let find_cli_path () : string option =
  let repo_root = Filename.dirname (Sys.getcwd ()) in
  let scope =
    List.fold_left Filename.concat repo_root [ "nodejs"; "node_modules"; "@github" ]
  in
  let existing path = if Sys.file_exists path then Some path else None in
  let from_env =
    match Sys.getenv_opt "COPILOT_CLI_PATH" with
    | Some path when path <> "" -> existing path
    | _ -> None
  in
  let legacy_platform_index () =
    if Sys.file_exists scope && Sys.is_directory scope then
      Sys.readdir scope
      |> Array.to_list
      |> List.sort compare
      |> List.filter (fun name ->
             String.length name > 8 && String.sub name 0 8 = "copilot-")
      |> List.find_map (fun name ->
             existing (Filename.concat (Filename.concat scope name) "index.js"))
    else None
  in
  match from_env with
  | Some path -> Some path
  | None ->
    (match existing (Filename.concat (Filename.concat scope "copilot") "npm-loader.js") with
     | Some path -> Some path
     | None ->
       (match legacy_platform_index () with
        | Some path -> Some path
        | None -> existing (Filename.concat (Filename.concat scope "copilot") "index.js")))

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
  (* Point SDK clients at the discovered CLI; the SDK runs .js entry points
     with node. *)
  (match find_cli_path () with
   | Some cli ->
     Printf.printf "[ocaml-e2e] Using Copilot CLI at %s\n%!" cli;
     Unix.putenv "COPILOT_CLI_PATH" cli
   | None -> Printf.printf "[ocaml-e2e] Copilot CLI not found; set COPILOT_CLI_PATH\n%!");
  { pid = 0; url }

let stop_proxy proxy =
  ignore proxy;
  (* The proxy process gets cleaned up when the pipe closes *)
  ()

let proxy_url proxy = proxy.url
