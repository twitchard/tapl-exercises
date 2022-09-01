open Syntax
open Syntax.Debruijn

let parse (s: string): term =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.token lexbuf in
  Syntax.tag_debruijn ast

let rec typ_eq (a: typ) (b: typ): bool = match (a,b) with
  | (Nat, Nat) -> true
  | (Fn (a', a''), Fn(b', b'')) -> typ_eq a' a'' && typ_eq b' b''
  | _ -> false

type value =
  | ValInt of int
  | ValVar of string
  | ValAbs of string * term

let string_of_value (v : value) =
  match v with
    | ValInt i -> string_of_int i
    | ValVar x -> x
    | ValAbs (x, t) -> "(\\" ^ x ^ "." ^ string_of_term t
^ ")"

let rec substitute' (replacement : term) (t: term) (depth: int) =
    let recurse = substitute' replacement
    in
    match t with
    | TmZero | TmFree _ -> t
    | TmSucc t' -> TmSucc (recurse t' depth)
    | TmPred t' -> TmPred (recurse t' depth)
    (* x here is just a label for humans, debruijn-indexed lambda abstractions
       don't need names because the variables they bind in the body refer to them
       by distance, not by name  *)
    | TmAbs (x, ty, t') -> TmAbs (x, ty, recurse t' (depth + 1))
    | TmApp (a, b) -> TmApp (recurse a depth, recurse b depth)
    | TmBound (x, i) -> if i - depth == 1 then replacement else TmBound (x, i)
    | TmLet _ -> failwith "unexpected: TmLet should have been desugared"

  and substitute replacement t = substitute' replacement t 0

let %expect_test "substitute" = 
  let run x y = substitute x y |> string_of_term |> print_string in
    run TmZero (TmBound("x", 1));
    [%expect "0"];
    run TmZero (TmAbs("x", Nat, TmBound("x", 2)));
    [%expect "(\\x: Nat.0)"];
    run TmZero (TmAbs("x", Nat, TmBound("x", 3)));
    [%expect "(\\x: Nat.x3)"];
    run
      (TmAbs("x", Nat, TmAbs("y", Nat, TmApp(TmBound("x", 2), TmBound("y", 1)))))
      (TmAbs("y", Nat, TmApp(TmBound("x", 2), TmBound("y", 1))));
    [%expect "(\\y: Nat.(\\x: Nat.(\\y: Nat.x2 y1)) y1)"];;

let isValue (t: term): bool = match t with
  | TmApp(_) -> false
  | _ -> true


type evalResult =  
  | Value of term
  | Stuck of string
  | Next of term

let bindR r f = match r with
  | Value v -> Value (f v)
  | _ -> r

let rec eval1 (t: term): evalResult =
  let stuck desc = Stuck ("stuck (" ^ desc ^ "): " ^ string_of_term t) in
  match t with
  | TmZero | TmFree _ | TmAbs _ | TmBound _ -> Value t
  | TmPred TmZero -> Value TmZero
  | TmSucc x -> begin match eval1 x with
    | Value v -> Value (TmSucc v)
    | Next t' -> Next (TmSucc t')
    | Stuck s -> Stuck s
  end
  | TmPred x -> begin match eval1 x with
    | Value v -> Value (TmPred v)
    | Next t' -> Next (TmPred t')
    | Stuck s -> Stuck s
  end
  | TmApp (t1, t2) -> begin match eval1 t1 with
    | Value (TmAbs(_name, _, body)) -> Next(body |> substitute t2)
    | Value _ -> stuck "bad application"
    | Next t1' -> Next (TmApp(t1',t2))
    | Stuck s -> Stuck s
  end
  | TmLet _ -> failwith "Unexpected: TmLet should have been desugared"

let rec evalNat (t: term): (int, string) Result.t =
  let open Result in
  let (>>=) = bind in
  match t with
    | TmZero -> ok 0
    | TmPred (TmSucc t') -> evalNat t'
    | TmPred _ -> ok 0
    | TmSucc t' -> evalNat t' >>= fun t'' -> (t'' + 1 |> ok)
    | _ -> Error ("Unexpected: evalNat called with " ^ string_of_term t)

let rec eval (t: term): (value, string) Result.t =
  let open Result in
  let (>>=) = bind in
  match eval1 t with
  | Value (TmZero) | Value (TmSucc _) | Value (TmPred _) -> evalNat t >>= (fun i -> ValInt i |> ok)
  | Value (TmAbs (name,_ty,body)) -> ValAbs (name,body) |> ok
  | Value (TmFree name) -> ValVar (name) |> ok
  | Value (TmBound _) -> Error ("Unexpected: eval1 returned a bound variable as a value for " ^ string_of_term t)
  | Value (TmApp _) -> Error ("Unexpected: eval1 returned an application as a value for " ^ string_of_term t)
  | Stuck msg -> Error msg
  | Next t' -> eval t'
  | TmLet _ -> failwith "Unexpected: TmLet should have been desugared"

type typeError = string

let rec check' (ctx: typ List.t) (t: term): (typ, typeError) Result.t =
  let open Result in
  let (>>=) = bind in
  match t with
  | TmZero -> ok Nat
  | TmFree(x) -> Error ("unbound variable: " ^ x)
  | TmSucc t' -> check' ctx t' >>= fun ty -> begin
    match ty with
    | Nat -> ok Nat
    | _ -> Error ("type error: succ applied to non-Nat" ^ string_of_term t')
  end
  | TmPred t'  -> check' ctx t' >>= begin function
    | Nat -> ok Nat
    | _ -> Error ("type error: pred applied to non-Nat" ^ string_of_term t')
  end

  | TmAbs (_name, typ, body) -> check' (typ::ctx) body >>= fun typ' -> ok (Fn(typ, typ'))
  | TmApp (a, b) -> check' ctx a >>= begin function
    | Fn (a', a'') as a_typ -> check' ctx b >>= fun b' -> if typ_eq b' a' then ok a'' else Error ("Cannot apply function of type " ^ string_of_typ a_typ ^ " to term " ^ string_of_term b ^ " of type " ^ string_of_typ b')
    | Nat -> Error ("type error: nat is not a function: " ^ string_of_term a)
  end
  | TmBound (name, i) -> begin match List.nth_opt ctx (i-1) with
    | None -> Error ("Bound variable " ^ name ^ " could not be resolved in the context in term: " ^ string_of_term t)
    | Some typ' -> ok typ'
  end
  | TmLet _ -> failwith "Unexpected: TmLet should have been desugared"

and check t = check' [] t

let %expect_test "check" =
  let run x = parse x |> check |> fun x -> begin
    match x with Ok typ' -> string_of_typ typ' | Error err -> err
  end |> print_string
  in
    run "0";
    [%expect "Nat"];
    run "succ 0";
    [%expect "Nat"];
    run "\\x: Nat.0";
    [%expect "Nat -> Nat"];
    run "\\x: Nat.x";
    [%expect "Nat -> Nat"];
    run "\\x: Nat -> Nat .x";
    [%expect "(Nat -> Nat) -> Nat -> Nat"];;


let %expect_test "eval1" = 
  let run x = parse x |> eval1 |> fun x -> begin
    match x with
    | Stuck err -> err
    | Value t | Next t -> string_of_term t
  end |> print_string in
  run "(\\x: Nat.\\y: Nat.x y)(\\x: Nat.\\y: Nat.\\z: Nat.x y z)";
  [%expect "(\\y: Nat.(\\x: Nat.(\\y: Nat.(\\z: Nat.x3 y2 z1))) y1)"];;

let %expect_test "eval" = 
  let run x = parse x |> eval |> fun x -> begin
    match x with
    | Error err -> err
    | Ok r -> string_of_value r
  end |> print_string in
    run "(\\x: Nat.x) y";
    [%expect "y"];
    run "(\\x: Nat.x) (\\x: Nat.x)";
    [%expect "(\\x.x1)"];
    run "(\\x: Nat. x (x 0)) (\\z: Nat. succ z)";
    [%expect "2"];
    run "succ 0";
    [%expect "1"];
    run "(\\x: Nat.\\y: Nat. x(x y)) (\\x: Nat.succ x) (succ 0)";
    [%expect "3"];
    run "pred (succ 0)";
    [%expect "0"];
    run "succ (pred 0)";
    [%expect "1"];
  ;;

(*
ctx, x:t |- x:t
--------

*)
