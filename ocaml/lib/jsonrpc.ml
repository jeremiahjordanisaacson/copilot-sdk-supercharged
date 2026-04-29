(* Copyright (c) Microsoft Corporation. All rights reserved. *)

(** JSON-RPC 2.0 transport layer with Content-Length header framing. *)

(* ========================================================================== *)
(* Error types                                                                *)
(* ========================================================================== *)

type jsonrpc_error = {
  code : int;
  message : string;
  data : Yojson.Safe.t option;
}

exception Jsonrpc_error of jsonrpc_error
exception Process_exited of string

type request_handler = string -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t

(* ========================================================================== *)
(* Internal state                                                             *)
(* ========================================================================== *)

type t = {
  ic : Lwt_io.input_channel;
  oc : Lwt_io.output_channel;
  mutable next_id : int;
  pending : (int, Yojson.Safe.t Lwt.u) Hashtbl.t;
  handlers : (string, request_handler) Hashtbl.t;
  mutable notification_handler :
    (string -> Yojson.Safe.t -> unit Lwt.t) option;
  mutable running : bool;
  write_mutex : Lwt_mutex.t;
}

(* ========================================================================== *)
(* Construction                                                               *)
(* ========================================================================== *)

let create ic oc =
  { ic
  ; oc
  ; next_id = 1
  ; pending = Hashtbl.create 16
  ; handlers = Hashtbl.create 8
  ; notification_handler = None
  ; running = false
  ; write_mutex = Lwt_mutex.create ()
  }

(* ========================================================================== *)
(* Wire format: Content-Length header framing                                  *)
(* ========================================================================== *)

let read_message (ic : Lwt_io.input_channel) : Yojson.Safe.t option Lwt.t =
  let open Lwt.Syntax in
  let rec read_headers content_length =
    let* line = Lwt_io.read_line_opt ic in
    match line with
    | None -> Lwt.return_none
    | Some "" | Some "\r" ->
      (* End of headers *)
      (match content_length with
       | None -> Lwt.return_none
       | Some len ->
         let buf = Bytes.create len in
         let* () = Lwt_io.read_into_exactly ic buf 0 len in
         let body = Bytes.to_string buf in
         let json = Yojson.Safe.from_string body in
         Lwt.return_some json)
    | Some header ->
      let header = String.trim header in
      let cl =
        if String.length header > 16
           && String.lowercase_ascii (String.sub header 0 16)
              = "content-length: "
        then
          Some (int_of_string (String.trim (String.sub header 16
                 (String.length header - 16))))
        else content_length
      in
      read_headers cl
  in
  read_headers None

let write_message (oc : Lwt_io.output_channel) (mutex : Lwt_mutex.t)
    (json : Yojson.Safe.t) : unit Lwt.t =
  let open Lwt.Syntax in
  let body = Yojson.Safe.to_string json in
  let header =
    Printf.sprintf "Content-Length: %d\r\n\r\n" (String.length body)
  in
  Lwt_mutex.with_lock mutex (fun () ->
    let* () = Lwt_io.write oc header in
    let* () = Lwt_io.write oc body in
    Lwt_io.flush oc)

(* ========================================================================== *)
(* Message reading loop                                                       *)
(* ========================================================================== *)

let handle_response (t : t) (id : int) (result : Yojson.Safe.t option)
    (error : Yojson.Safe.t option) : unit =
  match Hashtbl.find_opt t.pending id with
  | None -> ()
  | Some resolver ->
    Hashtbl.remove t.pending id;
    (match error with
     | Some err ->
       let open Yojson.Safe.Util in
       let code = try err |> member "code" |> to_int with _ -> -1 in
       let message =
         try err |> member "message" |> to_string with _ -> "Unknown error"
       in
       let data =
         try Some (err |> member "data") with _ -> None
       in
       Lwt.wakeup_exn resolver
         (Jsonrpc_error { code; message; data })
     | None ->
       let value = match result with Some v -> v | None -> `Null in
       Lwt.wakeup resolver value)

let handle_request (t : t) (id : Yojson.Safe.t) (method_ : string)
    (params : Yojson.Safe.t) : unit Lwt.t =
  let open Lwt.Syntax in
  match Hashtbl.find_opt t.handlers method_ with
  | None ->
    let resp =
      `Assoc
        [ ("jsonrpc", `String "2.0")
        ; ("id", id)
        ; ("error",
           `Assoc
             [ ("code", `Int (-32601))
             ; ("message", `String (Printf.sprintf "Method not found: %s" method_))
             ])
        ]
    in
    write_message t.oc t.write_mutex resp
  | Some handler ->
    let* result =
      Lwt.catch
        (fun () ->
          let* r = handler method_ params in
          Lwt.return_ok r)
        (fun exn -> Lwt.return_error (Printexc.to_string exn))
    in
    let resp =
      match result with
      | Ok value ->
        `Assoc
          [ ("jsonrpc", `String "2.0")
          ; ("id", id)
          ; ("result", value)
          ]
      | Error msg ->
        `Assoc
          [ ("jsonrpc", `String "2.0")
          ; ("id", id)
          ; ("error",
             `Assoc
               [ ("code", `Int (-32603))
               ; ("message", `String msg)
               ])
          ]
    in
    write_message t.oc t.write_mutex resp

let handle_notification (t : t) (method_ : string) (params : Yojson.Safe.t)
    : unit Lwt.t =
  match t.notification_handler with
  | None -> Lwt.return_unit
  | Some handler -> handler method_ params

let read_loop (t : t) : unit Lwt.t =
  let open Lwt.Syntax in
  let rec loop () =
    if not t.running then Lwt.return_unit
    else
      let* msg = read_message t.ic in
      match msg with
      | None ->
        (* EOF -- mark pending requests as failed *)
        t.running <- false;
        Hashtbl.iter
          (fun _id resolver ->
            Lwt.wakeup_exn resolver (Process_exited "CLI process exited"))
          t.pending;
        Hashtbl.reset t.pending;
        Lwt.return_unit
      | Some json ->
        let open Yojson.Safe.Util in
        let id_opt =
          try Some (json |> member "id") with _ -> None
        in
        let method_opt =
          try Some (json |> member "method" |> to_string) with _ -> None
        in
        let result_opt =
          try Some (json |> member "result") with _ -> None
        in
        let error_opt =
          try
            let e = json |> member "error" in
            if e = `Null then None else Some e
          with _ -> None
        in
        let* () =
          match id_opt, method_opt with
          | Some (`Int id), None ->
            (* Response to our request *)
            handle_response t id result_opt error_opt;
            Lwt.return_unit
          | Some id, Some method_ ->
            (* Incoming request *)
            handle_request t id method_ (try json |> member "params" with _ -> `Null)
          | None, Some method_ ->
            (* Incoming notification *)
            handle_notification t method_ (try json |> member "params" with _ -> `Null)
          | _ -> Lwt.return_unit
        in
        loop ()
  in
  loop ()

(* ========================================================================== *)
(* Public API                                                                 *)
(* ========================================================================== *)

let start (t : t) : unit Lwt.t =
  t.running <- true;
  Lwt.async (fun () ->
    Lwt.catch (fun () -> read_loop t) (fun _exn ->
      t.running <- false;
      Lwt.return_unit));
  Lwt.return_unit

let stop (t : t) : unit Lwt.t =
  let open Lwt.Syntax in
  t.running <- false;
  Hashtbl.iter
    (fun _id resolver ->
      Lwt.wakeup_exn resolver (Process_exited "Client stopped"))
    t.pending;
  Hashtbl.reset t.pending;
  let* () =
    Lwt.catch (fun () -> Lwt_io.close t.oc) (fun _ -> Lwt.return_unit)
  in
  Lwt.catch (fun () -> Lwt_io.close t.ic) (fun _ -> Lwt.return_unit)

let send_request (t : t) (method_ : string) (params : Yojson.Safe.t)
    : Yojson.Safe.t Lwt.t =
  if not t.running then
    Lwt.fail (Process_exited "Client is not running");
  let id = t.next_id in
  t.next_id <- t.next_id + 1;
  let promise, resolver = Lwt.wait () in
  Hashtbl.add t.pending id resolver;
  let msg =
    `Assoc
      [ ("jsonrpc", `String "2.0")
      ; ("id", `Int id)
      ; ("method", `String method_)
      ; ("params", params)
      ]
  in
  let open Lwt.Syntax in
  let* () = write_message t.oc t.write_mutex msg in
  promise

let send_notification (t : t) (method_ : string) (params : Yojson.Safe.t)
    : unit Lwt.t =
  let msg =
    `Assoc
      [ ("jsonrpc", `String "2.0")
      ; ("method", `String method_)
      ; ("params", params)
      ]
  in
  write_message t.oc t.write_mutex msg

let register_handler (t : t) (method_ : string) (handler : request_handler)
    : unit =
  Hashtbl.replace t.handlers method_ handler

let set_notification_handler (t : t)
    (handler : string -> Yojson.Safe.t -> unit Lwt.t) : unit =
  t.notification_handler <- Some handler

let is_running (t : t) : bool = t.running
