(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91))


let fresh_var used_vars : string = 
  if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
  then raise (OutOfVariablesError)
  else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)


let rec fv = ????
(*
  ADD FUNCTIONS BELOW
*) 

let extract_some = function
  | Some x -> x 

let rec substitute x s = ????
(*
  ADD FUNCTIONS BELOW
*) 


let rec reduce_cbv = ????
(*
  ADD FUNCTIONS BELOW
*) 

let rec reduce_cbn = ????
(*
  ADD FUNCTIONS BELOW
*) 