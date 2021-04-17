open Json_encoding
module Cal = CalendarLib.Date

type hex = Hex.t [@encoding conv Hex.show (fun s -> `Hex s) string]
[@@deriving encoding, jsoo]

type tsp = Cal.t [@encoding conv CalendarLib.Printer.Date.to_string CalendarLib.Printer.Date.from_string string]
           [@class_type "Ezjs_min.date"]
           [@conv ((fun js -> Cal.from_unixfloat (js##getTime /. 1000.)),
                   (fun c -> new%js Ezjs_min.date_fromTimeValue (1000. *. Cal.to_unixfloat c)))]
[@@deriving encoding, jsoo]

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
} [@@deriving encoding, jsoo]

type episode = {
  e_id: int;
  e_title: string;
  e_season: int;
  e_episode: int;
  e_date: tsp;
  e_code: string;
  e_global: int;
  e_user: episode_user;
  e_status: string; [@exclude "unknown"]
} [@@deriving encoding {ignore}, jsoo]

type show = {
  s_id: int;
  s_imdb_id: string option;
  s_title: string;
  s_genres: (string * string) list; [@assoc] [@dft []]
  s_images: (string * string option) list; [@assoc] [@dft []]
  s_outdated: bool; [@exclude false]
  s_description: string; [@dft ""]
  s_creation: string; [@dft ""]
  s_in_account: bool; [@dft false]
  s_seasons: string; [@dft ""]
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
type display_episode = (show [@obj1 "episode"]) [@@deriving encoding {ignore}]
type search_shows = (show list [@obj1 "shows"]) [@@deriving encoding {ignore}]
type episodes = (episode list [@obj1 "episodes"]) [@@deriving encoding {ignore}]
