type term =
  | Int of int
  | Var of string
  | Lambda of term * term
  | App of term * term

type ttype =
  | TInt
  | TVar of string
  | TApp of ttype * ttype

type typed_children =
  | TypedChildrenInt
  | TypedChildrenVar
  | TypedChildrenLambda of typed_term * typed_term
  | TypedChildrenApp of typed_term * typed_term

and typed_term =
  | TypedTerm of term * ttype * typed_children

and constr =
  | Equality of ttype * ttype

let symbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let c = ref 0
let gensym () = incr c ; Char.escaped (String.get symbols (!c-1))

let rec term_type_of_term t = match t with
  | Int(_) -> [(t,TInt)]
  | Var(_) -> [(t,TVar(gensym()))]
  | Lambda(Var(s),body) ->
     (t,TVar(gensym()))::(Var(s),TVar(gensym()))::(term_type_of_term body)
  | Lambda (_,_) -> failwith "Parse error"
  | App(t1,t2) ->
     (t,TVar(gensym()))::(term_type_of_term t1)@(term_type_of_term t2)

let rec typed_term_of_term t = match t with
  | Int(_) -> TypedTerm (t,TInt,TypedChildrenInt)
  | Var(_) -> TypedTerm (t, TVar(gensym()), TypedChildrenVar)
  | Lambda(Var(s),body) -> TypedTerm(t, TVar(gensym()),
                                  TypedChildrenLambda(typed_term_of_term(Var s),
                                                      typed_term_of_term(body)))
  | Lambda (_,_) -> failwith "Parse error"
  | App(t1,t2) -> TypedTerm(t,TVar(gensym()),
                            TypedChildrenApp(typed_term_of_term t1,
                                             typed_term_of_term t2))

let rec constr_of_typed_term term = match term with

  | TypedTerm(Int(_),_,TypedChildrenInt) -> []

  | TypedTerm(Var(_),_,TypedChildrenVar) -> []

  | TypedTerm(Lambda(_,_),type1,TypedChildrenLambda(typed_term1,typed_term2)) ->
     (match typed_term1, typed_term2 with
      | (TypedTerm(_,type2,_), TypedTerm(_,type3,_)) ->
         Equality(type1,TApp(type2,type3))::(constr_of_typed_term typed_term1)@(constr_of_typed_term typed_term2))

  | TypedTerm(App(_,_),type1,TypedChildrenApp(typed_term1,typed_term2)) ->
     (match typed_term1, typed_term2 with
      | (TypedTerm(_,type2,_),TypedTerm(_,type3,_)) ->
         Equality(type2,TApp(type3,type1))::(constr_of_typed_term typed_term1)@(constr_of_typed_term typed_term2))
  | _ -> failwith "Parse error"

(*************)
(* STRING OF *)
(*************)

let rec nspaces = function
  | 0 -> ""
  | n -> "   "^(nspaces (n-1))

let rec string_of_term level = function
  | Int(n) -> (nspaces level)^"Int("^(string_of_int n)^")"
  | Var(s) -> (nspaces level)^"Var("^s^")"
  | Lambda(t1,t2) -> (nspaces level)^"Lambda(\n"^(string_of_term (level+1) t1)^",\n"^(string_of_term (level+1) t2)^")"
  | App(t1, t2) -> (nspaces level)^"App(\n"^(string_of_term (level+1) t1)^",\n"^(string_of_term (level+1) t2)^")"

let rec string_of_term2 = function
  | Int(n) -> "Int("^(string_of_int n)^")"
  | Var(s) -> "Var("^s^")"
  | Lambda(t1,t2) -> "Lambda("^(string_of_term2 t1)^", "^(string_of_term2 t2)^")"
  | App(t1, t2) -> "App("^(string_of_term2 t1)^", "^(string_of_term2 t2)^")"

let rec string_of_type = function
  | TInt -> "TInt"
  | TVar(s) -> "TVar("^s^")"
  | TApp(t1,t2) -> "TApp("^(string_of_type t1)^", "^(string_of_type t2)^")"

let rec string_of_typed_term level = function
  | TypedTerm(t,tt,tc) ->
     (nspaces level)^"TypedTerm(\n"^(string_of_term (level+1) t)^", "^(string_of_type tt)^",\n"^(string_of_typed_children (level+1) tc)^")"

and string_of_typed_children level = function
  | TypedChildrenInt -> (nspaces level)^"TypedChildrenInt"
  | TypedChildrenVar -> (nspaces level)^"TypedChildrenVar"
  | TypedChildrenLambda (tt1,tt2) -> (nspaces level)^"TypedChildrenLambda(\n"^(string_of_typed_term (level+1) tt1)^",\n"^(string_of_typed_term (level+1) tt2)^")"
  | TypedChildrenApp (tt1,tt2) -> (nspaces level)^"TypedChildrenApp(\n"^(string_of_typed_term (level+1) tt1)^",\n"^(string_of_typed_term (level+1) tt2)^")"


let string_of_constr = function
  | Equality(tt1,tt2) -> "Equality("^(string_of_type tt1)^", "^(string_of_type tt2)^")"

(********)
(* MAIN *)
(********)

let _ =
  (*let t1 = App(App(Lambda(Var "x",Lambda(Var "y",Var("y"))),Int(2)),Int(1)) in*)
  let ts = [
      App(App(Lambda(Var "x",Lambda(Var "y",Var("y"))),Int(2)),Int(1));
      (* Var("x");
       * Int(12);
       * Lambda(Var("x"),Var("x"));
       * Lambda(Var("x"),Lambda(Var("y"),Var("x"))); *)
    ] in
  List.map (fun t -> List.map (fun c -> print_endline(string_of_constr c)) (constr_of_typed_term (typed_term_of_term t))) ts;
  (*List.map (fun t -> print_endline (string_of_typed_term 0 (typed_term_of_term t));print_newline()) ts*)
  (*List.map (fun t -> List.map (function (t,tt) -> print_endline((string_of_term2 t)^" : "^(string_of_type tt))) (term_type_of_term t)) ts*)
