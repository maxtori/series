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
let get0 ?msg ?headers ?params base service =
  let msg = if !verbose then msg else None in
  get0 ?msg ?headers ?params base service
let post0 ?msg ?headers ?params base service =
  let msg = if !verbose then msg else None in
  post0 ?msg ?headers ?params ~input:() base service

[%%post {
  name="auth"; path="/members/auth"; output=auth_enc;
  params=[login_param; pwd_param]; errors } ]

[%%get {
  name="is_active"; path="/members/is_active";
  output=Json_encoding.unit;
  params=[login_param; pwd_param]; errors } ]

let hash_pwd pwd = Hex.(show @@ of_string @@ Digest.string pwd)

let handle = function
  | Ok x -> Ok x
  | Error (EzReq_lwt_S.KnownError {code; error}) -> Error (code, error)
  | Error (EzReq_lwt_S.UnknownError {code; msg}) ->
    Error (code, [{code=0; text=Option.value ~default:"" msg}])

let request_token ~login ~password =
  Lwt.map handle @@
  post0 ~msg:"auth" base auth ~headers:(headers None)
    ~params:[login_param, S login; pwd_param, S (hash_pwd password)]

let active_token token =
  Lwt.map (fun r -> match handle r with Error _ -> false | Ok () -> true) @@
  get0 ~msg:"is_active" base is_active ~headers:(headers (Some token))

(* EPISODES *)

let limit_param = Param.int "limit"
let released_param = Param.int "released"
let id_param = Param.int "id"
let show_id_param = Param.int "showId"
let season_param = Param.int "season"
let order_param = Param.string "order"
let status_param = Param.string "status"

[%%get {
  name="episodes"; path="/episodes/list"; errors;
  output=list_shows_enc; params=[limit_param; released_param; show_id_param] } ]

let optl p f = function None -> [] | Some x -> [p, f x]

let get_episodes ?(limit=1) ?(released=0) ?id token =
  let params = [limit_param, I limit; released_param, I released] @
               (optl show_id_param (fun x -> I x) id) in
  Lwt.map handle @@
  get0 ~msg:"episodes" base episodes ~headers:(headers (Some token)) ~params

[%%post {
  name="downloaded"; path="/episodes/downloaded"; errors;
  output=display_episode_enc; params=[id_param] } ]

[%%delete {
  name="undownloaded"; path="/episodes/downloaded"; errors;
  output=display_episode_enc; params=[id_param] } ]

let downloaded ~token id b =
  let params = [id_param, I id] in
  let s = if b then downloaded else undownloaded in
  Lwt.map handle @@
  post0 ~msg:"downloaded" base s ~headers:(headers (Some token)) ~params

[%%post {
  name="watched"; path="/episodes/watched"; errors;
  output=display_episode_enc; params=[id_param] } ]

[%%delete {
  name="unwatched"; path="/episodes/watched"; errors;
  output=display_episode_enc; params=[id_param] } ]

let watched ~token id b =
  let params = [id_param, I id] in
  let s = if b then watched else unwatched in
  Lwt.map handle @@
  post0 ~msg:"watched" base s ~headers:(headers (Some token)) ~params

(* SHOW *)

[%%get { name="show"; path="/shows/display"; errors; output=display_show_enc; params=[id_param] } ]

let get_show ?token id =
  Lwt.map handle @@
  get0 ~msg:"show" base show ~headers:(headers token) ~params:[id_param, I id]

let title_param = Param.string "title"

[%%get { name="search"; path="/shows/search"; errors; output=search_shows_enc; params=[title_param] } ]

let search_shows ?token title =
  Lwt.map handle @@
  get0 ~msg:"search" base search ~headers:(headers token) ~params:[title_param, S title]

[%%post { name="add_show"; path="/shows/show"; errors; output=display_show_enc; params=[id_param] } ]
[%%delete { name="remove_show"; path="/shows/show"; errors; output=display_show_enc; params=[id_param] } ]

let add_show ~token id =
  Lwt.map handle @@
  post0 ~msg:"add" base add_show ~headers:(headers (Some token))
    ~params:[id_param, I id]

let remove_show ~token id =
  Lwt.map handle @@
  post0 ~msg:"remove" base remove_show ~headers:(headers (Some token))
    ~params:[id_param, I id]

[%%get { name="discover"; path="/shows/discover"; errors; output=search_shows_enc } ]

let discover ?token () =
  Lwt.map handle @@
  get0 ~msg:"discover" base discover ~headers:(headers token)

[%%get {
  name="show_episodes"; path="/shows/episodes"; errors; output=episodes_enc;
  params=[id_param; season_param] } ]

let get_show_episodes ?token ?season id =
  let params = optl season_param (fun x -> I x) season in
  Lwt.map handle @@
  get0 ~msg:"show_episodes" base show_episodes ~headers:(headers token)
    ~params:((id_param, I id) :: params)

[%%get {
  name="my_shows"; path="/shows/member"; errors; output=search_shows_enc;
  params=[order_param; status_param] } ]

let my_shows ?(order="last_seen") ?(status="active") token =
  let params = [order_param, S order; status_param, S status] in
  Lwt.map handle @@
  get0 ~msg:"my_shows" base my_shows ~headers:(headers (Some token)) ~params

[%%post { name="archive"; path="/shows/archive"; errors; output=display_show_enc; params=[id_param] } ]
[%%delete { name="unarchive"; path="/shows/archive"; errors; output=display_show_enc; params=[id_param] } ]

let archive_show ~token id =
  Lwt.map handle @@
  post0 ~msg:"archive" base archive ~headers:(headers (Some token))
    ~params:[id_param, I id]

let unarchive_show ~token id =
  Lwt.map handle @@
  post0 ~msg:"unarchive" base unarchive ~headers:(headers (Some token))
    ~params:[id_param, I id]

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

let unseen_episode ?store ?(fill=true) ?(period=Cal.Period.day 8) ~token ~show ~id acc episodes =
  let now = Cal.today () in
  let get_show ffill =
    if not fill then rok show
    else match store with
      | None ->
        let|>? s = get_show ~token id in
        ffill s
      | Some (get, add, put, _delete) ->
        let> s = get id in
        match s with
        | Error _ ->
          let|>? s = get_show ~token id in
          let s = ffill s in
          put id s;
          s
        | Ok None ->
          let|>? s = get_show ~token id in
          let s = ffill s in
          add id s;
          s
        | Ok (Some s) -> rok s in
  match episodes with
  | [] -> rok acc
  | e :: _ ->
    let e = {e with e_title = format_filename e.e_title} in
    match e.e_date with
    | None -> rok acc
    | Some date ->
      if Cal.(Period.compare (sub date now) period) < 0 then
        let es_episode = Some {e with e_status = episode_status now date} in
        let ffill s = {show with s_images = s.s_images; s_genres = s.s_genres} in
        let|>? es_show = get_show ffill in
        {es_show; es_episode} :: acc
      else
        rok acc

let get_unseen ?(limit=1) ?(released=0) ?period ?store ?id ?fill ?order token =
  let>? shows = get_episodes ~limit ~released ?id token in
  let|>? l = fold (fun acc s ->
    unseen_episode ?store ?fill ?period ~token ~show:s.su_show ~id:s.su_show.s_id acc s.su_unseen
  ) [] shows in
  List.sort (compare_episode_show ?order) l

let run p = EzLwtSys.run (fun () -> print_error p)
