let (let>) = Lwt.bind
let (let|>) p f = Lwt.map f p
let (let>?) p f = Lwt.bind p (function Error e -> Lwt.return_error e | Ok x -> f x)
let (let|>?) p f = Lwt.map (Result.map f) p

let rok = Lwt.return_ok
let rerr = Lwt.return_error

let rec iter f = function
  | [] -> rok ()
  | h :: t ->
    Lwt.bind (f h) @@ function
    | Error e -> rerr e
    | Ok () -> iter f t

let rec fold f acc = function
  | [] -> rok acc
  | h :: t ->
    Lwt.bind (f acc h) @@ function
    | Error e -> rerr e
    | Ok acc -> fold f acc t
