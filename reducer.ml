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
  | Variable x -> StringSet.add x StringSet.empty 
  | Abstraction (x, t) -> StringSet.remove x (fv t)
  | Application (t1, t2) -> StringSet.union (fv t1) (fv t2)
;;

let extract_some = function
  | Some x -> x
;;

let rec substitute x s = fun t ->
  let free_vars_s = fv s in
  match t with
  | Variable y -> if y = x then s else Variable y 
  | Abstraction(y, t1) -> 
    if y = x then
      Abstraction(x, t1)
    else if (not (StringSet.mem y free_vars_s)) then
      Abstraction(y, substitute x s t1)
    (* here y <> x and y in FV(s) *)
    else     
      let free_vars_t1 = fv t1 in
      let new_var = fresh_var (StringSet.union free_vars_t1 free_vars_s) in 
      Abstraction(new_var, substitute x s (substitute y (Variable new_var) t1))
  | Application (t1, t2) -> Application (substitute x s t1, substitute x s t2)
;;

let rec reduce_cbv = function
  | Abstraction (_, _) -> None (* Base case: abstraction cannot be further reduced *)
  | Application (Abstraction (x, t1), t2) ->
    ( 
      match reduce_cbv t2 with
      | Some t2' -> Some (Application (Abstraction (x, t1), t2'))
      | None -> 
        (
          match t2 with 
          | Abstraction (_, _) -> Some (substitute x t2 t1)
          | Variable _ -> Some (substitute x t2 t1)
          | _ -> None
        )
    ) (* Reduction of application with abstraction *)
  | Application (t1, t2) ->
    (
      match reduce_cbv t1 with
      | Some t1' -> Some (Application (t1', t2))
      | None ->
        (match reduce_cbv t2 with
        | Some t2' -> Some (Application (t1, t2'))
        | None -> None)
    ) (* Non-abstraction application case *)
  | _ -> None (* Base case: variables cannot be further reduced *)
;;

let rec reduce_cbn = fun t ->
  match t with
  | Application (Abstraction (x, t1), t2) -> Some (substitute x t2 t1)
  | Application (t1, t2) ->
    (match reduce_cbn t1 with
    | Some t1' -> Some (Application (t1', t2))
    | None -> None) (* check if this is problematic*)
  | _ -> None
;;