open Rp
open Types
open EzAPI
open EzReq_lwt

let base = BASE "https://api.betaseries.com"

(* AUTH *)

let api_key = ref ""
let headers token = [
  "X-BetaSeries-Version", "3.0";
  "X-BetaSeries-Key", !api_key ] @
  match token with None -> [] | Some token -> [ "Authorization", "Bearer " ^ token ]

let login_param = Param.string "login"
let pwd_param = Param.string "password"
let errors = [
  Err.Case {code=400; name="error"; encoding=errors_enc;
            select=Option.some; deselect=(fun x -> x)} ]

let verbose = ref false

let handle = function
  | Ok x -> Ok x
  | Error (EzReq_lwt_S.KnownError {code; error}) -> Error (code, error)
  | Error (EzReq_lwt_S.UnknownError {code; msg}) ->
    Error (code, [{code=0; text=Option.value ~default:"" msg}])

let get0 ?msg ?headers ?params base service =
  let msg = if !verbose then msg else None in
  Lwt.map handle @@ get0 ?msg ?headers ?params base service
let post0 ?msg ?headers ?params base service =
  let msg = if !verbose then msg else None in
  Lwt.map handle @@ post0 ?msg ?headers ?params ~input:() base service

let%post auth = {
  path="/members/auth"; output=auth_enc;
  params=[login_param; pwd_param]; errors
}

let%get is_active = {
  path="/members/is_active"; output=Json_encoding.unit;
  params=[login_param; pwd_param]; errors
}

let hash_pwd pwd = Hex.(show @@ of_string @@ Digest.string pwd)

let request_token ~login ~password =
  post0 ~msg:"auth" base auth ~headers:(headers None)
    ~params:[login_param, S login; pwd_param, S (hash_pwd password)]

let active_token token =
  Lwt.map (function Ok () -> true | Error _ -> false) @@
  get0 ~msg:"is_active" base is_active ~headers:(headers (Some token))

(* EPISODES *)

let limit_param = Param.int "limit"
let released_param = Param.int "released"
let id_param = Param.int "id"
let show_id_param = Param.int "showId"
let season_param = Param.int "season"
let order_param = Param.string "order"
let status_param = Param.string "status"

let%get episodes = {
  path="/episodes/list"; errors; output=A.list_shows_enc;
  params=[limit_param; released_param; show_id_param]
}

let optl p f = function None -> [] | Some x -> [p, f x]

let get_episodes ?(limit=1) ?(released=0) ?id token =
  let params =
    [limit_param, I limit; released_param, I released] @
    (optl show_id_param (fun x -> I x) id) in
  get0 ~msg:"episodes" base episodes ~headers:(headers (Some token)) ~params

let%get episode = {
  path="/episodes/display"; errors; output=A.display_episode_enc;
  params=[id_param]
}

let get_episode ?token id =
  get0 ~msg:"episode" base episode ~headers:(headers token) ~params:[id_param, I id]

let%post downloaded = {
  path="/episodes/downloaded"; errors; output=A.display_episode_enc; params=[id_param]
}

[%%delete {
  name="undownloaded"; path="/episodes/downloaded"; errors;
  output=A.display_episode_enc; params=[id_param] } ]

let downloaded ~token id b =
  let params = [id_param, I id] in
  let s = if b then downloaded else undownloaded in
  post0 ~msg:"downloaded" base s ~headers:(headers (Some token)) ~params

let%post watched = { path="/episodes/watched"; errors; output=A.display_episode_enc; params=[id_param] }

let%delete unwatched = { path="/episodes/watched"; errors; output=A.display_episode_enc; params=[id_param] }

let watched ~token id b =
  let params = [id_param, I id] in
  let s = if b then watched else unwatched in
  post0 ~msg:"watched" base s ~headers:(headers (Some token)) ~params

(* SHOW *)

let%get show = { path="/shows/display"; errors; output=A.display_show_enc; params=[id_param] }

let get_show ?token id =
  get0 ~msg:"show" base show ~headers:(headers token) ~params:[id_param, I id]

let title_param = Param.string "title"

let%get search = { path="/shows/search"; errors; output=A.search_shows_enc; params=[title_param] }

let search_shows ?token title =
  get0 ~msg:"search" base search ~headers:(headers token) ~params:[title_param, S title]

let%post add_show = { path="/shows/show"; errors; output=A.display_show_enc; params=[id_param] }
let%delete remove_show = { path="/shows/show"; errors; output=A.display_show_enc; params=[id_param] }

let add_show ~token id =
  post0 ~msg:"add" base add_show ~headers:(headers (Some token))
    ~params:[id_param, I id]

let remove_show ~token id =
  post0 ~msg:"remove" base remove_show ~headers:(headers (Some token))
    ~params:[id_param, I id]

let%get discover = { path="/shows/discover"; errors; output=A.search_shows_enc }

let discover ?token () =
  get0 ~msg:"discover" base discover ~headers:(headers token)

let%get show_episodes = {
  path="/shows/episodes"; errors; output=A.episodes_enc;
  params=[id_param; season_param]
}

let get_show_episodes ?token ?season id =
  let params = optl season_param (fun x -> I x) season in
  get0 ~msg:"show_episodes" base show_episodes ~headers:(headers token)
    ~params:((id_param, I id) :: params)

let%get my_shows = {
  path="/shows/member"; errors; output=A.search_shows_enc;
  params=[order_param; status_param]
}

let my_shows ?(order="last_seen") ?(status="active") token =
  let params = [order_param, S order; status_param, S status] in
  get0 ~msg:"my_shows" base my_shows ~headers:(headers (Some token)) ~params

let%post archive = { path="/shows/archive"; errors; output=A.display_show_enc; params=[id_param] }
let%delete unarchive = { path="/shows/archive"; errors; output=A.display_show_enc; params=[id_param] }

let archive_show ~token id =
  post0 ~msg:"archive" base archive ~headers:(headers (Some token))
    ~params:[id_param, I id]

let unarchive_show ~token id =
  post0 ~msg:"unarchive" base unarchive ~headers:(headers (Some token))
    ~params:[id_param, I id]

(* PLANNING *)

let%get planning = { path="/planning/timeline"; errors; output=planning_enc }

let get_planning token =
  get0 ~msg:"planning" base planning ~headers:(headers (Some token))

(* MAIN *)

let print_error p =
  Lwt.map (Result.fold ~ok:(fun () -> ()) ~error:(fun (code, l) ->
    Format.eprintf "Error %d\n%s@." code
      (EzEncoding.construct ~compact:false Types.errors_enc l))) p

let episode_status now date =
  if Cal.compare date now <= 0 then "out"
  else if Cal.(Period.compare (sub date now) (Cal.Period.day 1)) <= 0 then "maybe"
  else "future"

let format_filename s =
  let s = Str.global_replace (Str.regexp ":") "," s in
  Str.global_replace (Str.regexp "<\\|>\\|\"\\|/\\|\\\\\\||\\|\\?\\|\\*") " " s

let format_show_title s =
  if String.get s (String.length s - 1) = ')' then
    let i = String.rindex s '(' in
    format_filename @@ String.trim @@ String.sub s 0 i
  else format_filename s

let compare_episode_show ?(order=`asc) s1 s2 =
  match s1.es_episode, s2.es_episode with
  | None, None -> String.compare s1.es_show.s_title s2.es_show.s_title
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some e1, Some e2 ->
    match e1.e_date, e2.e_date with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some d1, Some d2 ->
      match order with
      | `asc -> Cal.compare d1 d2
      | `desc ->
        let tomorrow = Cal.(next (today ()) `Day) in
        let t1 = Cal.compare d1 tomorrow in
        let t2 = Cal.compare d2 tomorrow in
        let t = Cal.compare d1 d2 in
        if t1 > 0 && t2 > 0 then t
        else if t1 > 0 then 1
        else if t2 > 0 then -1
        else -t

let update_show ?show s = match show with
  | None -> s
  | Some show -> {show with s_images = s.s_images; s_genres = s.s_genres}

let get_and_store_show ?store ?(fill=true) ?show ~token id =
  match fill, show, store with
  | false, Some show, _ -> rok show
  | _, _, None ->
    let|>? s = get_show ~token id in
    update_show ?show s
  | _, _, Some (get, add, put, _delete) ->
    let> s = get id in
    match s with
    | Error _ ->
      let|>? s = get_show ~token id in
      let s = update_show ?show s in
      put id s;
      s
    | Ok None ->
      let|>? s = get_show ~token id in
      let s = update_show ?show s in
      add id s;
      s
    | Ok (Some s) -> rok s

let unseen_episode ?store ?fill ?(period=Cal.Period.day 8) ~token ?(show: show option) ~id ~now e =
  let e = { e with e_title = format_filename e.e_title } in
  match e.e_date with
  | None -> rok None
  | Some date ->
    if Cal.(Period.compare (sub date now) period) < 0 then
      let es_episode = Some {e with e_status = episode_status now date} in
      let|>? es_show = get_and_store_show ?store ?fill ?show ~token id in
      Some {es_show; es_episode}
    else rok None

let get_unseen ?(limit=1) ?(released=0) ?period ?store ?id ?fill ?order token =
  let now = Cal.today () in
  let>? shows = get_episodes ~limit ~released ?id token in
  let>? planning = get_planning token in
  let>? planning = fold (fun acc d ->
    let events = List.filter_map (function Episode_release er -> Some er | _ -> None) d.events in
    fold (fun acc ev ->
      let>? e = get_episode ~token ev.er_id in
      if e.e_user.eu_seen then rok acc else
      let|>? e = unseen_episode ?store ?fill ?period ~token ~id:ev.er_show_id ~now e in
      match e with None -> acc | Some e -> e :: acc) acc events
  ) [] planning in
  let|>? episodes = fold (fun acc s ->
    match s.su_unseen with
    | [] -> rok acc
    | e :: _ ->
      let|>? e = unseen_episode ?store ?fill ?period ~token ~show:s.su_show ~id:s.su_show.s_id ~now e in
      match e with None -> acc | Some e -> e :: acc) [] shows in
  List.sort (compare_episode_show ?order) (episodes @ planning)

let run p = EzLwtSys.run (fun () -> print_error p)
