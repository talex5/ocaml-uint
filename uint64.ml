type t = Int64.t

let of_int = Int64.of_int
let to_int = Int64.to_int
let logxor = Int64.logxor
let zero = Int64.zero
let one = Int64.one
let add = Int64.add
let sub = Int64.sub

let of_int64 x = x
let to_int64 x = x

let compare a b  = Int64.(compare (sub a min_int) (sub b min_int))

let printer f x =
  Format.fprintf f "%Lu" x

let to_string x =
  Format.asprintf "%a" printer x

let to_string_hex x =
  Format.sprintf "0x%Lx" x

let of_string s =
  if String.length s > 2 && s.[0] = '0' && s.[1] = 'x' then
    Scanf.sscanf s "0x%Lx" (fun x -> x)
  else
    Scanf.sscanf s "%Lu" (fun x -> x)

let () =
  assert (-1L = of_string (to_string_hex (-1L)));
  assert (-1L = of_string (to_string (-1L)))

let min_int = 0L
let max_int = -1L
