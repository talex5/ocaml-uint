type t = Int32.t

let succ = Int32.succ
let of_int = Int32.of_int

let to_int x = (* XXX: 64-bit only *)
  let x = Int32.to_int x in
  if x >= 0 then x
  else x + 0x100000000

let logxor = Int32.logxor
let zero = Int32.zero
let one = Int32.one
let add = Int32.add
let sub = Int32.sub

let of_int32 x = x
let to_int32 x = x

let compare a b  = Int32.(compare (sub a min_int) (sub b min_int))

let printer f x =
  Format.fprintf f "%lu" x

let to_string x =
  Format.asprintf "%a" printer x

let to_string_hex x =
  Format.sprintf "0x%lx" x

let of_string s =
  if String.length s > 2 && s.[0] = '0' && s.[1] = 'x' then
    Scanf.sscanf s "0x%lx" (fun x -> x)
  else
    Scanf.sscanf s "%lu" (fun x -> x)

let () =
  assert (-1l = of_string (to_string_hex (-1l)));
  assert (-1l = of_string (to_string (-1l)))

let min_int = 0l
let max_int = -1l
