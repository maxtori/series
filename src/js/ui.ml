open Rp
open Ezjs_min
open Types
open Theme

type serie = {
  se_show : show;
  se_episodes : episode list; [@mutable]
  se_season : int option; [@mutable]
} [@@deriving jsoo]

type data = {
  shows : episode_show list; [@mutable]
  db : Ezjs_idb.Types.iDBDatabase t; [@mutable] [@ignore]
  token : string; [@mutable]
  path : string; [@mutable]
  query : string; [@mutable]
  series : show list; [@mutable]
  theme : theme; [@mutable]
  serie : serie option; [@mutable]
} [@@deriving jsoo]

module V = Vue_js.Make(struct
    class type data = data_jsoo
    class type all = data_jsoo
    let id = "app"
  end)

let copy _app show e =
  let show = show_of_jsoo show in
  let e = episode_of_jsoo e in
  let s = string @@ Format.sprintf "%s - %s - %s"
      (Api.format_show_title show.s_title) e.e_code_fmt (Api.format_filename e.e_title) in
  let clipboard = Unsafe.variable "navigator.clipboard" in
  Unsafe.(coerce clipboard)##writeText s |> ignore

let locked = ref false

let downloaded app e =
  if not !locked then
    let e = episode_of_jsoo e in
    Api.run @@
    let@! _ = Api.downloaded ~token:(to_string app##.token) e.e_id e.e_user.eu_downloaded in
    ()

let watched app e =
  if not !locked then
    let e = episode_of_jsoo e in
    Api.run @@
    let@! _ = Api.watched ~token:(to_string app##.token) e.e_id e.e_user.eu_seen in
    ()

let variant _app e =
  let e = episode_of_jsoo e in
  if e.e_status = "out" then string "primary"
  else if e.e_status = "maybe" then string "info"
  else string "secondary"

let update_show app s =
  let data = data_of_jsoo app in
  let s = show_of_jsoo s in
  Api.run @@
  let@! s2 = Api.get_show ~token:data.token s.s_id in
  let es_show = {s with s_images = s2.s_images; s_genres = s2.s_genres;
                        s_outdated = false} in
  Idb.put_show app##.db s.s_id es_show;
  List.iteri
    (fun i ss ->
       if ss.es_show.s_id = s.s_id then
         ignore @@ app##.shows##splice_1 i 1 (episode_show_to_jsoo {ss with es_show}))
    data.shows

let set_outdated _app s =
  s##.outdated := _true

let refresh_episode app id =
  locked := true;
  let data = data_of_jsoo app in
  Api.run @@
  let@ new_shows = Api.get_unseen ~store:(Idb.manage_show app##.db) ~id data.token in
  match new_shows with
  | [] ->
    let shows = of_listf episode_show_to_jsoo @@
      List.filter (fun s -> s.es_show.s_id <> id) data.shows in
    app##.shows := shows;
    Lwt.map (fun () -> locked := false; Ok ()) (EzLwtSys.sleep 1.)
  | {es_episode; _} :: _ ->
    List.iteri
      (fun i s -> if s.es_show.s_id = id then
          ignore @@ app##.shows##splice_1 i 1 (episode_show_to_jsoo {s with es_episode}))
      data.shows;
    Lwt.map (fun () -> locked := false; Ok ()) (EzLwtSys.sleep 1.)

let search app =
  app##.series := of_list [];
  Api.run @@
  let@! searches = Api.search_shows ~token:(to_string app##.token) (to_string app##.query) in
  app##.series := of_listf show_to_jsoo searches

let add_show app id =
  let data = data_of_jsoo app in
  Api.run @@
  let@! r = Api.add_show ~token:data.token id in
  Idb.add_show app##.db r.s_id r;
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    data.series

let remove_show app id =
  let data = data_of_jsoo app in
  Idb.remove_show app##.db id;
  Api.run @@
  let@! r = Api.remove_show ~token:data.token id in
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    data.series

let archive_show app id =
  let data = data_of_jsoo app in
  Api.run @@
  let@! r = Api.archive_show ~token:data.token id in
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    data.series

let unarchive_show app id =
  let data = data_of_jsoo app in
  Api.run @@
  let@! r = Api.unarchive_show ~token:data.token id in
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    data.series

let discover app =
  app##.series := of_list [];
  Api.run @@
  let@! series = Api.discover ~token:(to_string app##.token) () in
  app##.series := of_listf show_to_jsoo series

let set_body_class = function
  | None -> Dom_html.document##.body##removeAttribute (string "class")
  | Some c -> Dom_html.document##.body##setAttribute (string "class") (string c)

let change_theme app =
  let theme, s = if app##.theme##.body = undefined then dark_theme, "dark" else light_theme, "light" in
  app##.theme := theme_to_jsoo theme;
  set_body_class theme.t_body;
  Idb.update_config ~key:"theme" app##.db s

let serie app id =
  Api.run @@
  let@ se_show = Api.get_show ~token:(to_string app##.token) id in
  let season = int_of_string_opt se_show.s_seasons in
  let@! se_episodes = Api.get_show_episodes ~token:(to_string app##.token) ?season id in
  app##.serie := def (serie_to_jsoo {se_show; se_episodes; se_season=season})

let update_episodes app season =
  let serie = to_optdef serie_of_jsoo app##.serie in
  match serie with
  | None -> ()
  | Some serie ->
    Api.run @@
    let@! se_episodes = Api.get_show_episodes ~token:(to_string app##.token) ~season serie.se_show.s_id in
    match Optdef.to_option app##.serie with
    | None -> ()
    | Some serie ->
      serie##.season := def season;
      serie##.episodes := of_listf episode_to_jsoo se_episodes

let home app =
  Api.run @@
  let@! shows = Api.get_unseen ~store:(Idb.manage_show app##.db) (to_string app##.token) in
  app##.shows := of_listf episode_show_to_jsoo shows

let my_series app =
  app##.series := of_list [];
  Api.run @@
  let@! series = Api.my_shows (to_string app##.token) in
  app##.series := of_listf show_to_jsoo series

let file_path = ref false

let get_path () =
  match Url.url_of_string (to_string Dom_html.window##.location##.href) with
  | None -> ""
  | Some url -> match url with
    | Url.Http hu | Url.Https hu -> String.concat "/" hu.Url.hu_path
    | Url.File fu ->
      file_path := true;
      String.concat "/" fu.Url.fu_path

let set_path ?(scroll=true) l =
  if not !file_path then
    let path = some @@ string @@ "/" ^ String.concat "/" l in
    Dom_html.window##.history##pushState path (string "") path;
    if scroll then Dom_html.window##scroll 0 0

let route app path =
  app##.path := path;
  let l = String.split_on_char '/' @@ to_string path in
  let path, l = match l with
    | ["search"] -> search app; "search", l
    | ["discover"] -> discover app; "discover", l
    | ["my_series"] -> my_series app; "my_series", l
    | ["serie"; id] -> serie app (int_of_string id); "serie", l
    | _ -> home app; "home", [] in
  app##.path := string path;
  set_path l

let () =
  V.add_method2 "copy" copy;
  V.add_method1 "downloaded" downloaded;
  V.add_method1 "watched" watched;
  V.add_method1 "variant" variant;
  V.add_method1 "update_show" update_show;
  V.add_method1 "set_outdated" set_outdated;
  V.add_method1 "refresh_episode" refresh_episode;
  V.add_method1 "add_show" add_show;
  V.add_method1 "remove_show" remove_show;
  V.add_method1 "archive_show" archive_show;
  V.add_method1 "unarchive_show" unarchive_show;
  V.add_method0 "change_theme" change_theme;
  V.add_method1 "update_episodes" update_episodes;
  V.add_method1 "route" route;
  Idb.open_db @@ fun db ->
  Api.run @@
  let@ theme = Idb.get_config ~key:"theme" db in
  let@ token = Idb.get_config ~key:"token" db in
  let theme = match theme with
    | Some "dark" -> set_body_class @@ Some "bg-dark"; dark_theme
    | _ -> light_theme in
  let data = {
    shows = []; token=(match token with None -> "" | Some t -> t);
    db; path="home"; series = []; query=""; theme; serie = None } in
  let app = V.init ~data:(data_to_jsoo data) ~export:true ~show:true () in
  let@! token, changed = Api.get_token ?token () in
  app##.token := string token;
  if changed then Idb.update_config ~key:"token" db token;
  let path = get_path () in
  Dom_html.window##.onpopstate := Dom_html.handler (fun _e ->
      route app (string @@ get_path ());
      _true);
  route app (string path)
