open Types
open Jsoo
open Ezjs_min
open Ezjs_idb

type proxy = {
  p_name: string;
  p_url: string;
  p_for_anime: bool;
  p_with_code: bool;
  p_post_url: string;
} [@@deriving jsoo]

let dummy_proxy = {p_name=""; p_url=""; p_with_code=true; p_for_anime=false; p_post_url=""}

type version = {
  version: int;
  upgrade: Types.iDBDatabase t -> unit;
  downgrade: Types.iDBDatabase t -> unit;
}

let mk_version name version create = {
  version;
  upgrade = (fun db -> ignore @@ create db name);
  downgrade = (fun db -> db##deleteObjectStore (string name))
}

module ShowStore = Store(NoTr(struct type t = int end))(struct
    type js = show_jsoo t
    type t = show
    let to_js = show_to_jsoo
    let of_js = show_of_jsoo
  end)

module ConfigStore = Store(StringTr)(StringTr)

module ProxyStore = Store(StringTr)(struct
    type js = proxy_jsoo t
    type t = proxy
    let to_js = proxy_to_jsoo
    let of_js = proxy_of_jsoo
  end)

let versions = [ {
  version = 1;
  upgrade = (fun db ->
    ignore @@ ShowStore.create db;
    ignore @@ ConfigStore.create db;
    ignore @@ ProxyStore.create db);
  downgrade = (fun db ->
    db##deleteObjectStore (string "shows");
    db##deleteObjectStore (string "config");
    db##deleteObjectStore (string "proxies")) } ]

let upgrade db e =
  if e.new_version > e.old_version then
    List.iter (fun v ->
      if v.version > e.old_version && v.version <= e.new_version then v.upgrade db)
      versions
  else if e.new_version < e.old_version then
    List.iter (fun v ->
      if v.version > e.new_version && v.version <= e.old_version then v.downgrade db)
      (List.rev versions)

let open_db ?(version=List.length versions) f =
  let error r = match AOpt.to_option r with
    | None -> log "open_db error"
    | Some r ->
      log "open_db error (%s)" (to_string r##.name);
      js_log r##.message in
  ShowStore.set_name "shows";
  ConfigStore.set_name "config";
  ProxyStore.set_name "proxies";
  openDB ~upgrade ~error ~version "series" f

(* shows *)

let get_show db id =
  let st = ShowStore.store ~mode:READONLY db in
  let w, n = Lwt.wait () in
  ShowStore.get ~error:(fun _ -> Lwt.wakeup n (Error (0, []))) st
    (fun s -> Lwt.wakeup n (Ok s)) (ShowStore.K id);
  w

let add_show db id s =
  let st = ShowStore.store ~mode:READWRITE db in
  ShowStore.add ~key:id st s

let put_show db id s =
  let st = ShowStore.store ~mode:READWRITE db in
  ShowStore.put ~key:id st s

let remove_show db id =
  let st = ShowStore.store ~mode:READWRITE db in
  ShowStore.delete st (ShowStore.K id)

let manage_show db =
  get_show db, add_show db, put_show db, remove_show db

let get_shows db =
  let st = ShowStore.store ~mode:READONLY db in
  let w, n = Lwt.wait () in
  ShowStore.get_all ~error:(fun _ -> Lwt.wakeup n (Error (0, []))) st
    (fun l -> Lwt.wakeup n (Ok l));
  w

(* proxies *)

let get_proxy db name =
  let st = ProxyStore.store ~mode:READONLY db in
  let w, n = Lwt.wait () in
  ProxyStore.get ~error:(fun _ -> Lwt.wakeup n (Error (0, []))) st
    (fun s -> Lwt.wakeup n (Ok s)) (ProxyStore.K name);
  w

let add_proxy db p =
  let st = ProxyStore.store ~mode:READWRITE db in
  ProxyStore.add ~key:p.p_name st p

let put_proxy db p =
  let st = ProxyStore.store ~mode:READWRITE db in
  ProxyStore.put ~key:p.p_name st p

let remove_proxy db name =
  let st = ProxyStore.store ~mode:READWRITE db in
  ProxyStore.delete st (ProxyStore.K name)

let manage_proxy db =
  get_proxy db, add_proxy db, put_proxy db, remove_proxy db

let get_proxies db =
  let st = ProxyStore.store ~mode:READONLY db in
  let w, n = Lwt.wait () in
  ProxyStore.get_all ~error:(fun _ -> Lwt.wakeup n (Error (0, []))) st
    (fun l -> Lwt.wakeup n (Ok l));
  w

(* config *)

let get_config ~key db =
  let st = ConfigStore.store ~mode:READONLY db in
  let w, n = Lwt.wait () in
  ConfigStore.get ~error:(fun _ -> Lwt.wakeup n (Ok None)) st
    (fun s -> Lwt.wakeup n (Ok s)) (ConfigStore.K key);
  w

let update_config ~key db value =
  let st = ConfigStore.store ~mode:READWRITE db in
  ConfigStore.put ~key st value

let remove_config ~key db =
  let st = ConfigStore.store ~mode:READWRITE db in
  ConfigStore.(delete st (K key))
