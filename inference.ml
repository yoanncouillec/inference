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

let symbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let c = ref 0
let gensym () = incr c ; Char.escaped (String.get symbols (!c-1))

let rec typed_term_of_term t = match t with
  | Int(_) -> TypedTerm (t,TInt,TypedChildrenInt)
  | Var(_) -> TypedTerm (t, TVar(gensym()), TypedChildrenVar)
  | Lambda(Var(s),t) -> TypedTerm(t, TypedLambda(TypedVar(s, TVar(gensym())),
                                     typed_term_of_term t,
                                     TVar(gensym()))
  | Lambda (_,_) -> failwith "Parse error"
  | App(t1,t2) -> TypedApp(typed_term_of_term t1, typed_term_of_term t2,TVar(gensym()))

let rec nspaces = function
  | 0 -> ""
  | n -> "   "^(nspaces (n-1))

let rec string_of_term = function
  | Int(n) -> "Int("^(string_of_int n)^")"
  | Var(s) -> "Var("^s^")"
  | Lambda(t1,t2) -> "Lambda("^(string_of_term t1)^", "^(string_of_term t2)^")"
  | App(t1, t2) -> "App("^(string_of_term t1)^", "^(string_of_term t2)^")"

let rec string_of_type = function
  | TInt -> "int"
  | TVar(s) -> s
  | TApp(t1,t2) -> (string_of_type t1)^" -> "^(string_of_type t2)

let rec string_of_typed_term level = function
  | TypedInt(n,t) -> (nspaces level)^"TypedInt("^(string_of_int n)^", "^(string_of_type t)^")"
  | TypedVar(s,t) -> (nspaces level)^"TypedVar("^s^", "^(string_of_type t)^")"
  | TypedLambda(tt1,tt2,t) -> (nspaces level)^"TypedLambda(\n"^(string_of_typed_term (level+1) tt1)^",\n"^(string_of_typed_term (level+1) tt2)^",\n"^(nspaces (level+1))^(string_of_type t)^")"
  | TypedApp(tt1,tt2,t) -> (nspaces level)^"TypedApp(\n"^(string_of_typed_term (level+1) tt1)^",\n"^(string_of_typed_term (level+1) tt2)^",\n"^(nspaces (level+1))^(string_of_type t)^")"

let _ =
  let t1 = App(App(Lambda(Var "x",Lambda(Var "y",Var("y"))),Int(2)),Int(1)) in
  let tt1 = TApp(TVar("a"),TVar("b")) in
  print_endline (string_of_term t1);
  print_endline (string_of_type tt1);
  print_endline (string_of_typed_term 0 (typed_term_of_term t1));
