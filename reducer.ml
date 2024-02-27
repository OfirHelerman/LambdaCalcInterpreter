(*
  Reducers (interpreters) for lambda-calculus.
*)

open Utils
open Parser


exception OutOfVariablesError


let possible_variables = List.map (fun x -> char_to_string (char_of_int x)) ((range 97 123) @ (range 65 91));;


let fresh_var used_vars : string = 
  if StringSet.is_empty (StringSet.diff (string_set_from_list(possible_variables)) used_vars) 
  then raise (OutOfVariablesError)
  else StringSet.choose (StringSet.diff (string_set_from_list(possible_variables)) used_vars)
;;


let rec fv = fun e ->
  match e with
  | Variable x -> [x]
  | Abstraction (x, t) -> List.filter (fun v -> v <> x) (fv t)
  | Application (t1, t2) -> List.append (fv t1) (fv t2)
;;
(*
  ADD FUNCTIONS BELOW
*) 

let extract_some = function
  | Some x -> x
;;

let rec substitute x s = fun t ->
  let free_vars_s = fv s in
  match t with
  | Variable y when y = x -> s
  | Variable y -> Variable y
  | Abstraction(y, t1) when y = x -> Abstraction(x, t1)
  | Abstraction(y, t1) when not (List.mem y free_vars_s) -> Abstraction(y, substitute x s t1)
  | Abstraction(y, t1) -> (* here y <> x and y in FV(s) *) Abstraction("z", substitute x s (substitute y (Variable "z") t1))
  | Application (t1, t2) -> Application (substitute x s t1, substitute x s t2)
;;

let rec reduce_cbv = fun t ->
  match t with
  | Application (Abstraction (x, t1), t2) ->
    let v = reduce_cbv t2 in (
      match v with
    | None -> Some (substitute x t2 t1)
    | Some _ -> Some (Application (Abstraction (x, t1), substitute x (extract_some v) t1))
    )
  | _ -> None
  ;;


let rec reduce_cbn = fun t ->
  match t with 
  | Application (Abstraction (x, t1), t2) ->
    let v = reduce_cbn t2 in
    (match v with
    | None -> Some (substitute x t2 t1)
    | Some _ -> Some (Application (Abstraction (x, t1), (extract_some v))))
  | _ -> None