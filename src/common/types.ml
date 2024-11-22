open Json_encoding
module Cal = CalendarLib.Date

type hex = Hex.t [@encoding conv Hex.show (fun s -> `Hex s) string]
[@@deriving encoding, jsoo]

type tsp = Cal.t [@encoding conv CalendarLib.Printer.Date.to_string CalendarLib.Printer.Date.from_string string]
[@@deriving encoding]
[@@@jsoo
  class type tsp_jsoo = Ezjs_min.date
  let tsp_to_jsoo c = new%js Ezjs_min.date_fromTimeValue (1000. *. Cal.to_unixfloat c)
  let tsp_of_jsoo js = Cal.from_unixfloat (js##getTime /. 1000.)
  let tsp_jsoo_conv = tsp_to_jsoo, tsp_of_jsoo
]

type json = Json_repr.ezjsonm [@@deriving encoding]
[@@@jsoo
  type json_jsoo = Ezjs_min.Unsafe.top
  let json_to_jsoo = Js_json.js_of_json
  let json_of_jsoo = Js_json.json_of_js]

type user = {
  u_id: int;
  u_login: string;
  u_xp: int;
} [@@deriving encoding {ignore}]

type auth = {
  a_user: user;
  a_token: string;
  a_hash: hex;
} [@@deriving encoding {ignore}]

type error = {
  code: int;
  text: string;
} [@@deriving encoding]

type errors = error list [@obj1 "errors"] [@@deriving encoding {ignore}]

type episode_user = {
  eu_downloaded: bool; [@mutable]
  eu_seen: bool; [@mutable]
  eu_hidden: bool option; [@mutable]
} [@@deriving encoding {ignore}, jsoo]

type episode = {
  e_id: int;
  e_title: string;
  e_season: int;
  e_episode: int;
  e_date: tsp option;
  e_code: string;
  e_code_fmt: string; [@exclude ""]
  e_global: int;
  e_user: episode_user;
  e_status: string; [@exclude "unknown"]
} [@@deriving encoding {ignore}, jsoo]

let episode_enc =
  conv
    (fun x -> x)
    (fun e -> {e with e_code_fmt = Format.sprintf "%dx%02d" e.e_season e.e_episode})
    episode_enc

type show_user = {
  su_archived : bool;
  su_favorited : bool;
} [@@deriving encoding {ignore}, jsoo]

type show = {
  s_id: int;
  s_imdb_id: string option;
  s_title: string; [@mutable]
  s_genres: (string * string) list;
  [@dft []] [@assoc] [@encoding union [
  case (list (tup2 string string)) (function [] -> Some [] | _ -> None) (fun l -> l);
  case (assoc string) (fun l -> Some l) (fun l -> l)]]
  s_images: (string * [`url of string | `json of json]) list; [@assoc] [@dft []]
  s_outdated: bool; [@exclude false] [@mutable]
  s_description: string; [@dft ""]
  s_creation: string; [@dft ""]
  s_in_account: bool; [@dft false]
  s_seasons: string; [@dft ""]
  s_user: show_user; [@dft {su_archived=false; su_favorited=false}]
  s_aliases: (string * string) list; [@assoc] [@dft []]
  s_language: string option;
} [@@deriving encoding {ignore}, jsoo]

type show_unseen = {
  su_show : show [@merge];
  su_unseen : episode list; [@dft []]
} [@@deriving encoding]

type episode_show = {
  es_show : show;
  es_episode : episode option;
} [@@deriving encoding, jsoo]

module A = struct
  type list_shows = (show_unseen list [@obj1 "shows"]) [@@deriving encoding {ignore}]
  type display_show = (show [@obj1 "show"]) [@@deriving encoding {ignore}]
  type display_episode = (episode [@obj1 "episode"]) [@@deriving encoding {ignore}]
  type search_shows = (show list [@obj1 "shows"]) [@@deriving encoding {ignore}]
  type episodes = (episode list [@obj1 "episodes"]) [@@deriving encoding {ignore}]
end

type order = [ `desc | `asc ] [@@deriving jsoo {enum}]

let unknown_kind_enc =
  conv (fun s -> s, ()) fst @@
  merge_objs (obj1 (req "type" string)) unit

let string_int_enc = conv string_of_int int_of_string string

type episode_release_payload = {
  er_id: int;
  er_show_id: int;
  er_show_title: string;
  er_show_slug: string;
  er_title: string;
  er_code: string;
  er_season: int; [@encoding string_int_enc]
  er_episode: int; [@encoding string_int_enc]
} [@@deriving encoding {ignore}]

type timeline_event =
  | Episode_release of (episode_release_payload [@wrap "payload"]) [@kind] [@kind_label "type"]
  | Unknown of (string [@encoding unknown_kind_enc])
[@@deriving encoding]

type timeline_day = {
  date: tsp;
  density: string;
  events: timeline_event list; [@dft []]
} [@@deriving encoding]

type planning = (timeline_day list [@obj1 "days"]) [@@deriving encoding {ignore}]
