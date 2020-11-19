type term =
  | Int of int
  | Var of string
  | Lambda of string * term
  | App of term * term

type ty =
  | TyInt
  | TyVar of string
  | TyArrow of ty * ty

type context = (string * ty) list

type typed_term = term * ty

type constr = ty * ty

type result = typed_term list * context * constr list

let rec get (s:string) (ctx:context) : ty =
  match ctx with
  | [] -> failwith "binding not found"
  | (s',ty')::rest -> if s = s' then ty' else get s rest

let symbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let count = ref 0
let gensym () = incr count ; Char.escaped (String.get symbols (!count-1))

let rec deriv (ctx:context) (term:term) (ty1:ty) (constr_list:constr list) : result =
  match term with
  | App(t1, t2) ->
     let ty2 = TyVar(gensym()) in
     let ty3 = TyArrow(ty2,ty1) in
     (match (deriv ctx t1 ty3 constr_list, deriv ctx t2 ty2 constr_list) with
      | ((typed_term_list1, ctx1, constr1),
         (typed_term_list2, ctx2, constr2)) ->
         (term, ty1)::typed_term_list1@typed_term_list2,(ctx1@ctx2@ctx),(constr_list@constr1@constr2))
  | Int(_) ->
     [(term, ty1)], ctx, (ty1,TyInt)::constr_list
  | Var(s) ->
     [(term, ty1)], ctx, (ty1, get s ctx)::constr_list
  | Lambda(s,body) ->
     let sigma = TyVar(gensym()) in
     let tau = TyVar(gensym()) in
     let ty2 = TyArrow(sigma,tau) in
     (match (deriv ((s,sigma)::ctx) body tau (constr_list)) with
      | (typed_term_list1, ctx1, constr1) -> ((term,ty1)::typed_term_list1, ctx1, (ty1,ty2)::constr1))

let deriv_init t = deriv [] t (TyVar(gensym())) []

let rec string_of_term = function
  | Int(n) -> "Int("^(string_of_int n)^")"
  | Var(s) -> "Var("^s^")"
  | Lambda(s,body) -> "Lambda("^s^", "^(string_of_term body)^")"
  | App(t1,t2) -> "App("^(string_of_term t1)^", "^(string_of_term t2)^")"

let rec string_of_ty = function
  | TyInt -> "TyInt"
  | TyVar(s) -> "TyVar("^s^")"
  | TyArrow (ty1,ty2) -> "TyArrow("^(string_of_ty ty1)^", "^(string_of_ty ty2)^")"

let rec string_of_constr_list = function
  | [] -> "[]"
  | (ty1,ty2)::rest ->
     "[CONSTR"^(List.fold_left (fun accu -> function (ty1',ty2') -> accu^",("^(string_of_ty ty1)^"="^(string_of_ty ty2')^")") ("("^(string_of_ty ty1)^"="^(string_of_ty ty2)) rest)^"]"

let rec string_of_context = function
  | [] -> "[]"
  | (s,ty)::rest ->
     "[CTX"^(List.fold_left (fun accu -> function (s',ty') -> accu^",("^s'^":"^(string_of_ty ty')^")") ("("^s^":"^(string_of_ty ty)) rest)^"]"

let string_of_typed_term = function
  | (term, ty) -> "("^(string_of_term term)^":"^(string_of_ty ty)^")"

let rec string_of_typed_term_list = function
  | [] -> "[]"
  | typed_term::rest ->
     "[TERMS"^(List.fold_left (fun accu -> function typed_term -> accu^",("^(string_of_typed_term typed_term)) (string_of_typed_term typed_term) rest) ^"]"

let string_of_result = function
  | (typed_term_list, context, constr_list) ->
     (string_of_typed_term_list typed_term_list)^",\n"^(string_of_context context)^",\n"^(string_of_constr_list constr_list)

let _ =
  let t = Lambda("y", Var("y")) in
  let result = deriv_init t in
  print_endline(string_of_result result)
     


