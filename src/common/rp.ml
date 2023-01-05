let (let>) = Lwt.bind
let (let|>) p f = Lwt.map f p
let (let>?) p f = Lwt.bind p (function Error e -> Lwt.return_error e | Ok x -> f x)
let (let|>?) p f = Lwt.map (Result.map f) p

let rec iter f = function
  | [] -> Lwt.return_ok ()
  | h :: t ->
      Lwt.bind (f h) @@ function
      | Error e -> Lwt.return_error e
      | Ok () -> iter f t

let rec fold f acc = function
  | [] -> Lwt.return_ok acc
  | h :: t ->
      Lwt.bind (f acc h) @@ function
      | Error e -> Lwt.return_error e
      | Ok acc -> fold f acc t
