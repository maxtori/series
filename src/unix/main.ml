open Rp
open Types

let download_dir = ref (Filename.concat (Sys.getenv "HOME") "Downloads")
let output_dir = ref "."
let decompress = ref "7z"

let read_token ?(file=".token") () =
  try
    let ic = open_in file in
    let token = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some Hex.(show @@ of_string token)
  with _ -> None

let store_token ?(file=".token") token =
  let oc = open_out file in
  output_string oc (Hex.to_string (`Hex token));
  close_out oc

let get_token () =
  let scan_token () =
    Format.printf "Username:@.";
    let login = Scanf.scanf "%s" (fun s -> s) in
    Format.printf "Password:@.";
    let password = Scanf.scanf "%s" (fun s -> s) in
    let@! auth = Api.request_token ~login ~password in
    store_token auth.a_token;
    auth.a_token in
  match read_token () with
  | Some token ->
    let> active = Api.active_token token in
    if active then Lwt.return_ok token
    else scan_token ()
  | None -> scan_token ()

let file_info f =
  match String.rindex_opt f '.' with
  | None -> None
  | Some i ->
    let ext = String.sub f (i+1) (String.length f - i - 1) in
    match String.rindex_from_opt f i '-' with
    | Some j ->
      let wname = String.sub f (j+1) (i-j-1) in
      Some (ext, wname)
    | None ->
      match String.rindex_from_opt f i '.' with
      | Some j ->
        let wname = String.sub f (j+1) (i-j-1) in
        Some (ext, wname)
      | None -> None

let handle_episode_files s e =
  let r_title = Str.global_replace (Str.regexp " ") "." s.s_title in
  let r_episode = e.e_code in
  let r = Str.regexp_case_fold (".*" ^ r_title ^ ".*" ^ r_episode ^ ".*") in
  let l = Array.to_list @@ Sys.readdir !download_dir in
  match List.find_opt (fun s -> Str.string_match r s 0) l with
  | None -> Format.printf "Cannot find files for %s %s@." s.s_title e.e_code
  | Some f ->
    let code = Sys.command @@ Format.sprintf "%s e %s" !decompress (Filename.concat !download_dir f) in
    if code <> 0 then Format.printf "Unrar failed for %s %s@." s.s_title e.e_code
    else
      let l = Array.to_list @@ Sys.readdir "." in
      match List.find_opt (fun s -> Str.string_match r s 0) l with
      | None -> Format.printf "Cannot find extracted file for %s %s@." s.s_title e.e_code
      | Some f ->
        match file_info f with
        | None -> Format.printf "Cannot get file info from %s@." f
        | Some (ext, warez) ->
          Sys.rename f @@ Filename.concat !output_dir
            (Format.sprintf "%s - %dx%02d - %s.%s.%s"
               s.s_title e.e_season e.e_episode e.e_title warez ext)

let main () =
  Api.print_error @@
  let@ token = get_token () in
  let@! shows = Api.get_unseen ~fill:false token in
  List.iter (fun s ->
      match s.es_episode with
      | None -> ()
      | Some e ->
        if e.e_user.eu_downloaded then handle_episode_files s.es_show e
    ) shows

let () =
  let specs = [
    "--dir", Arg.Set_string download_dir,
    "directory containing downloaded files (default: $HOME/Downloads)";
    "-d", Arg.Set_string download_dir,
    "alias of --dir";
    "--output", Arg.Set_string output_dir,
    "output directory (default: .)";
    "-o", Arg.Set_string download_dir,
    "alias of --output";
    "--command", Arg.Set_string decompress,
    "decompress program path";
    "-c", Arg.Set_string decompress,
    "alias of --command";
  ] in
  Arg.parse specs (fun _ -> ()) "Series";
  EzLwtSys.run main
