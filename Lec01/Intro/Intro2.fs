(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

//Extended in accordance to 1.1 (iv)
type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

//Added for use of example expressions
let e4 = Prim("max", Var "a", CstI 5);;

let e5 = Prim("min", Var "a", CstI 5);;

let e5 = Prim("==", Var "a", CstI 314);;


(* Evaluation within an environment *)

//Function extended according to 1.1 (i)
let rec evalOrig e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("max", e1, e2) -> if eval e1 env > eval e2 env then eval e1 env else eval e2 env 
    | Prim("min", e1, e2) -> if eval e1 env < eval e2 env then eval e1 env else eval e2 env
    | Prim("==", e1, e2) -> if eval e1 env == eval e2 env then 1 else 0
    | Prim _            -> failwith "unknown primitive";;

//The eval function rewritten after 1.1 (iii) and extended for 1.1 (v)
let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ope, e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "*" -> i1 * i2
        | "max" -> if i1 > i2 then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | "==" -> if i1 == i2 then 1 else 0
        | _ -> failwith "unknown primitive"
    | If(e1, e2, e3)    -> if eval e1 env <> 0 then eval e2 env else eval e3 env
        

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

//example expressions according to 1.1 (ii)
let eMax = eval e4 [("a", 314)];; 

let eMin = eval e5 [("a", 314)];;

let eEquals = eval e6 [("a", 314)];;

// added in 1.2 (i)
type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr

// added for 1.2 (ii)
let ae1 = Sub("v", Add(Var "w",Var "z"))

let ae2 = Mul(CstI 2, ae1)

let ae3 = Add(Add(Var "x", Var "y"), Add(Var "z", Var "v"))

// added for 1.2 (iii)
let rec fmt (a: aexpr) : string =
    match a with
    | CstI i -> sprintf "%A" i
    | Var x -> x
    | Add (a1,a2) -> sprintf "(%A + %A)" (fmt a1) (fmt a2)
    | Mul (a1, a2) -> sprintf "(%A * %A)" (fmt a1) (fmt a2)
    | Sub (a1, a2) -> sprintf "(%A - %A)" (fmt a1) (fmt a2)

// added for 1.2 (iv)
let rec simplify (a: aexpr) : aexpr =
    match a with
    | Add (a1,a2) ->
        let e1 = simplify a1
        let e2 = simplify a2
        match e1, e2 with
        | CstI 0, e2 -> e2
        | e1, CstI 0 -> e1
        | _ -> a
    | Mul (a1, a2) ->
        let e1 = simplify a1
        let e2 = simplify a2
        match e1, e2 with
        | CstI 1, e2 -> e2
        | e1, CstI 1 -> e1
        | CstI 0, e2 -> CstI 0
        | e1, CstI 0 -> CstI 0
        | _ -> a
    | Sub (a1, a2) ->
        let e1 = simplify a1
        let e2 = simplify a2
        match e1, e2 with
        | e1, CstI 0 -> e1
        | e1, e2 when e1 == e2 -> CstI 0
        | _ -> a
    | _ -> a

// added for 1.2 (v)
let rec symDiff (a: aexpr) x : aexpr = 
    match a with
    | CstI i -> CstI 0
    | Var e -> CstI 1
    | CstI y when y <> x -> CstI 0
    | Add (a1,a2) -> Add((symDiff a1 x),(symDiff a2 x))
    | Sub (a1,a2) -> Sub((symDiff a1 x),(symDiff a2 x))
    | Mul (a1,a2) -> Add (Mul(symDiff a1 x, a2),Mul(symDiff a2 x, a1))

