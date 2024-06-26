open Rp
open Types
open Api

type command =
  | Episodes
  | Episode of int
  | Show of int
  | Discover
  | My_shows
  | Planning
[@@deriving arg]

type config = {
  login: string;
  password: string;
  key: string;
  command: command;
} [@@deriving arg {exe="test.exe"}]

let print enc x =
  Format.printf "%s@." @@ EzEncoding.construct ~compact:false enc x

let () =
  Lwt_main.run @@ print_error @@
  let config = parse_config () in
  api_key := config.key;
  let>? auth = request_token ~login:config.login ~password:config.password in
  match config.command with
  | Episodes ->
    let|>? l = get_episodes auth.a_token in
    print A.list_shows_enc l
  | Planning ->
    let|>? p = get_planning auth.a_token in
    print planning_enc p
  | Episode id ->
    let|>? e = get_episode ~token:auth.a_token id in
    print episode_enc e
  | _ -> rok ()
