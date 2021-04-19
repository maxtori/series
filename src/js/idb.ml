open Types
open Ezjs_min
open Ezjs_idb

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

let db = "series"
let shows = "shows"
let config = "config"

let versions = [ {
    version = 1;
    upgrade = (fun db ->
        ignore @@ ShowStore.create db shows;
        ignore @@ ConfigStore.create db config);
    downgrade = (fun db ->
        db##deleteObjectStore (string config);
        db##deleteObjectStore (string shows)) } ]

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
  let error r = js_log r##.error in
  openDB ~upgrade ~error ~version db f

(* shows *)

let get_show db id =
  let st = ShowStore.store ~mode:READONLY db shows in
  let w, n = Lwt.wait () in
  ShowStore.get ~error:(fun _ -> Lwt.wakeup n (Error (0, []))) st
    (fun s -> Lwt.wakeup n (Ok s)) (ShowStore.K id);
  w

let add_show db id s =
  let st = ShowStore.store ~mode:READWRITE db shows in
  ShowStore.add ~key:id st s

let put_show db id s =
  let st = ShowStore.store ~mode:READWRITE db shows in
  ShowStore.put ~key:id st s

let remove_show db id =
  let st = ShowStore.store ~mode:READWRITE db shows in
  ShowStore.delete st (ShowStore.K id)

let manage_show db =
  get_show db, add_show db, put_show db, remove_show db

let get_shows db =
  let st = ShowStore.store ~mode:READONLY db shows in
  let w, n = Lwt.wait () in
  ShowStore.get_all ~error:(fun _ -> Lwt.wakeup n (Error (0, []))) st
    (fun l -> Lwt.wakeup n (Ok l));
  w

(* config *)

let get_config ~key db =
  let st = ConfigStore.store ~mode:READONLY db config in
  let w, n = Lwt.wait () in
  ConfigStore.get ~error:(fun _ -> Lwt.wakeup n (Ok None)) st
    (fun s -> Lwt.wakeup n (Ok s)) (ConfigStore.K key);
  w

let update_config ~key db value =
  let st = ConfigStore.store ~mode:READWRITE db config in
  ConfigStore.put ~key st value
