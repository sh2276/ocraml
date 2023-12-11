(* A vector is represented as a float array. *)
type t = float array

(* Checks if two vectors have the same length. *)
let assert_eq_len v1 v2 =
  if Array.length v1 <> Array.length v2 then
    invalid_arg "Vectors must have the same length"

(* Initalizes a vector from a float array.*)
let init (lst : float array) = lst

(*Adds two vectors of the same length.*)
let add (v1 : t) (v2 : t) =
  assert_eq_len v1 v2;
  Array.map2 ( +. ) v1 v2

(* Subtracts two vectors of the same length. *)
let sub (v1 : t) (v2 : t) =
  assert_eq_len v1 v2;
  Array.map2 ( -. ) v1 v2

(*Multiplies a vector by a scalar.*)
let scalar_mult (s : float) (v : t) = Array.map (fun a -> a *. s) v

(*Computes the dot prodcut of two vectors.*)
let dot_prod (v1 : t) (v2 : t) : float =
  assert_eq_len v1 v2;
  let prod = Array.mapi (fun i x -> x *. v2.(i)) v1 in
  Array.fold_left ( +. ) 0.0 prod

(* Infix operations (addition, subtraction, scalar multiplication, dot
   product). *)
let ( + ) = add
let ( - ) = sub
let ( * ) = scalar_mult
let ( @ ) = dot_prod

(* Length of a vector. *)
let length (v : t) = Array.length v

(* Converts a vector to a float array. *)
let to_array (v1 : t) = v1

(*Index of the largest element in the vector.*)
let argmax = function
  | [||] -> failwith "Empty vector"
  | arr ->
      let _, max_idx, _ =
        Array.fold_left
          (fun (max_score, max_idx, cur_idx) score ->
            if score > max_score then Stdlib.(score, cur_idx, cur_idx + 1)
            else Stdlib.(max_score, max_idx, cur_idx + 1))
          (arr.(0), 0, 0)
          arr
      in
      max_idx

(*Converts a vector into a string.*)
let to_string (v : t) =
  let str_elements = Array.map string_of_float (to_array v) in
  "[ " ^ String.concat "; " (Array.to_list str_elements) ^ " ]"
