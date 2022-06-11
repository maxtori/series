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
  s_images: (string * string option) list; [@assoc] [@dft []]
  s_outdated: bool; [@exclude false]
  s_description: string; [@dft ""]
  s_creation: string; [@dft ""]
  s_in_account: bool; [@dft false]
  s_seasons: string; [@dft ""]
  s_user: show_user; [@dft {su_archived=false; su_favorited=false}]
  s_aliases: (string * string) list [@assoc] [@dft []]
} [@@deriving encoding {ignore}, jsoo]

type show_unseen = {
  su_show : show [@merge];
  su_unseen : episode list; [@dft []]
} [@@deriving encoding]

type episode_show = {
  es_show : show;
  es_episode : episode option;
} [@@deriving encoding, jsoo]

type list_shows = (show_unseen list [@obj1 "shows"]) [@@deriving encoding {ignore}]
type display_show = (show [@obj1 "show"]) [@@deriving encoding {ignore}]
type display_episode = (episode [@obj1 "episode"]) [@@deriving encoding {ignore}]
type search_shows = (show list [@obj1 "shows"]) [@@deriving encoding {ignore}]
type episodes = (episode list [@obj1 "episodes"]) [@@deriving encoding {ignore}]
