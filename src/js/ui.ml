open Rp
open Ezjs_min_lwt
open Types
open Jsoo
open Theme

type serie = {
  se_show : show;
  se_episodes : episode list; [@mutable]
  se_season : int option; [@mutable]
} [@@deriving jsoo]

type login = {
  username : string; [@mutable]
  password : string; [@mutable]
} [@@deriving jsoo]

module CopyButton = struct

  let%prop tx = "light"
  and show : show = {req}
  and episode : episode = {req}
  and placement = "top"

  let%data copied = false

  let apply app tp b =
    app##.copied := bool b;
    Opt.iter tp (fun tp ->
      let msg = if b then "copied!" else "copy" in
      ignore @@ tp##setContent (Unsafe.obj [| ".tooltip-inner", Unsafe.inject (string msg) |]))

  let%meth copy app =
    let tp = Unsafe.global##.bootstrap##._Tooltip##getInstance [%el app] in
    let s = string @@ Format.sprintf "%s - %s - %s"
        (Api.format_show_title (to_string app##.show##.title)) (to_string app##.episode##.code_fmt_)
        (Api.format_filename @@ to_string app##.episode##.title) in
    ignore @@ Unsafe.global##.self##.navigator##.clipboard##writeText s;
    apply app tp true;
    ignore @@ Dom_html.window##setTimeout
      (wrap_callback @@ fun () -> apply app tp false)
      5000.

  [%%mounted fun app ->
    let cs : _ constr = Unsafe.global##.bootstrap##._Tooltip in
    let _ = new%js cs [%el app] in
    ()]

  [%%comp {conv}]

  {%%template|
   <button @click="copy()" :class="'btn btn-outline-'+tx" data-bs-toggle="tooltip" :data-bs-placement="placement" data-bs-trigger="hover" data-bs-title="copy" data-bs-container="#app">
     <i :class="copied ? 'bi bi-clipboard-check-fill' : 'bi bi-clipboard'"></i>
   </button>
  |}
end

module LoadButton = struct

  let button_class_f variant = "btn btn-" ^ variant
  let text_class_f = function
    | "primary" | "secondary" | "success" | "danger" | "dark" -> "text-light"
    | "outline-primary" -> "text-primary"
    | "outline-secondary" -> "text-secondary"
    | "outline-success" -> "text-success"
    | "outline-danger" -> "text-danger"
    | "outline-warning" -> "text-warning"
    | "outline-info" -> "text-info"
    | _ -> ""

  let%prop variant = "secondary"
  and grow = false
  and [@noconv] action : unit -> unit Promise.promise t = {req}

  let%data loading = false
  and button_style = ""
  and spinner_kind app : string = if to_bool app##.grow then "spinner-grow" else "spinner-border"
  and button_class app : string = button_class_f (to_string app##.variant)
  and text_class app : string = text_class_f (to_string app##.variant)

  let%watch variant app n _old =
    app##.button_class_ := string (button_class_f n);
    app##.text_class_ := string (text_class_f n)

  let%meth process app =
    app##.loading := _true;
    (app##.action ())##then_ (wrap_callback (fun () -> app##.loading := _false))

  [%%mounted fun app ->
    match Optdef.to_option [%el app] with
    | None -> ()
    | Some elt ->
      let width = elt##.offsetWidth + 1 in
      let height = elt##.offsetHeight + 1 in
      app##.button_style_ := string (Format.sprintf "width: %dpx; height: %dpx" width height)
  ]

  [%%comp {conv}]

  {%%template|
   <button @click="process()" :class="button_class" :style="button_style">
     <span v-if="!loading">
       <slot>
       </slot>
     </span>
     <span v-else :class="spinner_kind+' '+spinner_kind+'-sm '+text_class" role="status">
     </span>
   </button>
  |}

end

let locked = ref false

let%data shows : episode_show list = []
and path = "home"
and query = ""
and series : show list = []
and serie : serie option = None
and login : login = {username=""; password=""}
and proxy : Idb.proxy = Idb.dummy_proxy

let clear ?(query=true) app =
  app##.serie := undefined;
  app##.series := of_list [];
  if query then app##.query := string ""

let serie (app: all t) (id: int) =
  clear app;
  Api.run @@
  let>? show = Idb.get_show app##.db id in
  let>? se_show = match show with
    | None ->
      let|>? show = Api.get_show ~token:(to_string app##.token) id in
      if show.s_in_account then Idb.put_show app##.db show.s_id show;
      show
    | Some s -> Lwt.return_ok s in
  let season = int_of_string_opt se_show.s_seasons in
  let>? se_show, season = match season with
    | None ->
      let|>? show = Api.get_show ~token:(to_string app##.token) id in
      if show.s_in_account then Idb.put_show app##.db show.s_id show;
      show, int_of_string_opt show.s_seasons
    | Some season -> Lwt.return_ok (se_show, Some season) in
  let|>? se_episodes = Api.get_show_episodes ~token:(to_string app##.token) ?season id in
  app##.serie := def (serie_to_jsoo {se_show; se_episodes; se_season=season})

let set_body_class = function
  | None -> Dom_html.document##.body##removeAttribute (string "class")
  | Some c -> Dom_html.document##.body##setAttribute (string "class") (string c)

let file_path = ref false
let set_file_path () =
  match Url.url_of_string (to_string Dom_html.window##.location##.href) with
  | Some (Url.File _) -> file_path := true
  | _ -> ()

let set_path ?(scroll=true) ?(args=[]) s =
  if not !file_path then
    let args = match args with
      | [] -> ""
      | l -> "?" ^ String.concat "&" @@ List.map (fun (k, v) -> k ^ "=" ^ v) l in
    let path = some @@ string @@ "/" ^ s ^ args in
    Dom_html.window##.history##pushState path (string "") path;
    if scroll then Dom_html.window##scroll 0 0

let get_path () =
  match Url.url_of_string (to_string Dom_html.window##.location##.href) with
  | None -> "", []
  | Some url -> match url with
    | Url.Http hu | Url.Https hu -> String.concat "/" hu.Url.hu_path, hu.Url.hu_arguments
    | Url.File fu -> String.concat "/" fu.Url.fu_path, []


let%meth copy _app (show: show) (e: episode) (id : int) =
  let tp = Unsafe.global##.bootstrap##._Tooltip##getInstance (string ("#copy-" ^ string_of_int id)) in
  let s = string @@ Format.sprintf "%s - %s - %s"
      (Api.format_show_title show.s_title) e.e_code_fmt (Api.format_filename e.e_title) in
  ignore @@ Unsafe.global##.self##.navigator##.clipboard##writeText s;
  match Opt.to_option tp with
  | None -> ()
  | Some tp ->
    ignore @@ tp##setContent (Unsafe.obj [| ".tooltip-inner", Unsafe.inject (string "copied!") |])

and downloaded app (e: episode_jsoo t) =
  Promise.promise_lwt @@
  if not !locked then
    let b = not (to_bool e##.user##.downloaded) in
    Api.print_error @@
    let|>? _ = Api.downloaded ~token:(to_string app##.token) e##.id b in
    e##.user##.downloaded := bool b
  else Lwt.return_unit
[@@noconv]

and watched app (e: episode_jsoo t) =
  Promise.promise_lwt @@
  if not !locked then
    Api.print_error @@
    let b = not (to_bool e##.user##.seen) in
    let|>? _ = Api.watched ~token:(to_string app##.token) e##.id b in
    e##.user##.seen := bool b
  else Lwt.return_unit
[@@noconv]

and variant _app (e: episode) : string =
  if e.e_status = "out" then "primary"
  else if e.e_status = "maybe" then "info"
  else "outline-secondary"

let%meth rec update_show app (s: show) (reset: bool) =
  Api.run @@
  let|>? s2 = Api.get_show ~token:(to_string app##.token) s.s_id in
  let es_show = {s2 with s_title = s.s_title; s_outdated = false} in
  Idb.put_show app##.db s.s_id es_show;
  List.iteri
    (fun i ss ->
       if ss.es_show.s_id = s.s_id then
         ignore @@ app##.shows##splice_1 i 1 (episode_show_to_jsoo {ss with es_show}))
    (to_listf episode_show_of_jsoo app##.shows);
  if reset then serie app s.s_id else ()

and update_shows app =
  let shows = to_listf episode_show_of_jsoo app##.shows in
  List.iter (fun s -> if s.es_show.s_outdated then update_show app s.es_show false) shows

let%meth set_outdated _app (s: show_jsoo t) =
  s##.outdated := _true [@@noconv]

and refresh_episode app (id: int) =
  locked := true;
  Api.run @@
  let order = order_of_jsoo app##.order in
  let>? new_shows = Api.get_unseen ~store:(Idb.manage_show app##.db) ~id ~order (to_string app##.token) in
  match new_shows with
  | [] ->
    let shows = of_listf episode_show_to_jsoo @@
      List.filter (fun s -> s.es_show.s_id <> id) (to_listf episode_show_of_jsoo app##.shows) in
    app##.shows := shows;
    Lwt.map (fun () -> locked := false; Ok ()) (EzLwtSys.sleep 1.)
  | {es_episode; _} :: _ ->
    List.iteri
      (fun i s -> if s.es_show.s_id = id then
          ignore @@ app##.shows##splice_1 i 1 (episode_show_to_jsoo {s with es_episode}))
      (to_listf episode_show_of_jsoo app##.shows);
    Lwt.map (fun () -> locked := false; Ok ()) (EzLwtSys.sleep 1.)

and add_show app (id: int) =
  Api.run @@
  let|>? r = Api.add_show ~token:(to_string app##.token) id in
  Idb.add_show app##.db r.s_id r;
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    (to_listf show_of_jsoo app##.series)

and remove_show app (id: int) =
  Idb.remove_show app##.db id;
  Api.run @@
  let|>? r = Api.remove_show ~token:(to_string app##.token) id in
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    (to_listf show_of_jsoo app##.series)

and archive_show app (id: int) =
  Api.run @@
  let|>? r = Api.archive_show ~token:(to_string app##.token) id in
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    (to_listf show_of_jsoo app##.series)

and unarchive_show app (id: int) =
  Api.run @@
  let|>? r = Api.unarchive_show ~token:(to_string app##.token) id in
  List.iteri
    (fun i s -> if s.s_id = id then
        ignore @@ app##.series##splice_1 i 1 (show_to_jsoo r))
    (to_listf show_of_jsoo app##.series)

and switch_theme app : unit =
  let theme, s = if app##.theme##.bg = string "light" then dark_theme, "dark" else light_theme, "light" in
  app##.theme := theme_to_jsoo theme;
  set_body_class (Some ("bg-" ^ theme.t_bg));
  Idb.update_config ~key:"theme" app##.db s

and change_order app : unit =
  let order = match order_of_jsoo app##.order with `asc -> `desc | `desc -> `asc in
  Idb.update_config ~key:"order" app##.db (match order with `asc -> "asc" | `desc -> "desc");
  let shows = to_listf episode_show_of_jsoo app##.shows in
  let shows = List.sort (Api.compare_episode_show ~order) shows in
  app##.order := order_to_jsoo order;
  app##.shows := of_listf episode_show_to_jsoo shows

and update_episodes app (season: int) =
  let serie = to_optdef serie_of_jsoo app##.serie in
  match serie with
  | None -> ()
  | Some serie ->
    Api.run @@
    let|>? se_episodes = Api.get_show_episodes ~token:(to_string app##.token) ~season serie.se_show.s_id in
    match Optdef.to_option app##.serie with
    | None -> ()
    | Some serie ->
      serie##.season := def season;
      serie##.episodes := of_listf episode_to_jsoo se_episodes

and change_title app (s: js_string t) : unit =
  match Optdef.to_option app##.serie with
  | None -> ()
  | Some serie ->
    serie##.show##.title := s;
    let show = show_of_jsoo serie##.show in
    Idb.put_show app##.db show.s_id {show with s_title = to_string s} [@@noconv]

let search (app: all t) =
  clear ~query:false app;
  Api.run @@
  let|>? searches = Api.search_shows ~token:(to_string app##.token) (to_string app##.query) in
  app##.series := of_listf show_to_jsoo searches

let discover (app: all t) =
  clear app;
  Api.run @@
  let|>? series = Api.discover ~token:(to_string app##.token) () in
  app##.series := of_listf show_to_jsoo series

let my_series (app: all t) =
  clear app;
  Api.run @@
  let|>? series = Api.my_shows (to_string app##.token) in
  app##.series := of_listf show_to_jsoo series

let home (app: all t) =
  clear app;
  Api.run @@
  let order = order_of_jsoo app##.order in
  let|>? shows = Api.get_unseen ~store:(Idb.manage_show app##.db) ~order (to_string app##.token) in
  app##.shows := of_listf episode_show_to_jsoo shows

let%meth route app path id =
  app##.path := path;
  let path = to_string path in
  let args = match path with
    | "search" -> search app; []
    | "discover" -> discover app; []
    | "my_series" -> my_series app; []
    | "login" | "settings" -> []
    | "serie" ->
      begin match Optdef.to_option id with
        | None -> home app; app##.path := string "home"; []
        | Some id -> serie app id; ["id", string_of_int id]
      end
    | _ -> home app; app##.path := string "home"; [] in
  set_path ~args path

let%meth sign_out app =
  Idb.remove_config ~key:"token" app##.db;
  route app (string "login") undefined

and sign_in app =
  let login = login_of_jsoo app##.login in
  Api.run @@
  let|>? auth = Api.request_token ~login:login.username ~password:login.password in
  app##.token := (string auth.a_token);
  Idb.update_config ~key:"token" app##.db auth.a_token;
  route app (string "home") undefined

and add_proxy app =
  Idb.add_proxy app##.db (Idb.proxy_of_jsoo app##.proxy);
  ignore @@ app##.proxies##splice_1 app##.proxies##.length 0 app##.proxy;
  app##.proxy := Idb.(proxy_to_jsoo dummy_proxy)

and remove_proxy app name i =
  Idb.remove_proxy app##.db (to_string name);
  app##.proxies##splice i 1

and update_resolution app : unit =
  Idb.update_config ~key:"resolution" app##.db (to_string app##.resolution)

[%%mounted (fun app ->
  let f () =
    let path, args = get_path () in
    let id = optdef int_of_string @@ List.assoc_opt "id" args in
    route app (string path) id in
  Dom_html.window##.onpopstate := Dom_html.handler (fun _e -> f (); _true);
  set_file_path ();
  match to_string app##.token with
  | "" -> Lwt.return_ok @@ route app (string "login") undefined
  | token ->
    let> active = Api.active_token token in
    Lwt.return_ok @@
    if active then (
      app##.token := string token;
      f ())
    else (
      Idb.remove_config ~key:"token" app##.db;
      route app (string "login") undefined))]

let () =
  Idb.open_db @@ fun db ->
  Api.run @@
  let>? theme = Idb.get_config ~key:"theme" db in
  let theme = match theme with
    | Some "dark" -> set_body_class @@ Some "bg-dark"; dark_theme
    | _ -> light_theme in
  let>? token = Idb.get_config ~key:"token" db in
  let>? resolution = Idb.get_config ~key:"resolution" db in
  let>? order = Idb.get_config ~key:"order" db in
  let resolution = Option.value ~default:"" resolution in
  let>? proxies = Idb.get_proxies db in
  let%data db : Ezjs_idb.Types.iDBDatabase t = db [@@noconv]
  and token : string = Option.value ~default:"" token
  and theme : theme = theme
  and proxies : Idb.proxy list = proxies
  and resolution = resolution
  and order : order = match order with None | Some "asc" -> `asc | _ -> `desc in
  let _app = [%app {conv; types; mount; unhide; export; components=[CopyButton; LoadButton]}] in
  Lwt.return_ok ()
