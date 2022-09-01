type typ = 
  | Nat
  | Fn of typ * typ

let rec
  string_of_typ (ty: typ) = match ty with
  | Nat -> "Nat"
  | Fn (a, b) -> string_of_typ a ^ " -> " ^ string_of_typ b

module Named = struct
  type term =
      | TmZero
      | TmSucc of term
      | TmPred of term
      | TmAbs of string * typ * term
      | TmApp of term * term
      | TmVar of string
      | TmLet of string * typ * term * term
end

module Debruijn = struct
  type term =
      | TmZero
      | TmSucc of term
      | TmPred of term
      | TmAbs of string * typ * term
      | TmApp of term * term
      | TmFree of string
      | TmBound of string * int
      | TmLet of string * typ * term * term

  let rec
    string_of_term (t : term) = 
    match t with 
      | TmZero -> "0"
      | TmSucc t' -> "succ(" ^ string_of_term t' ^ ")"
      | TmPred t' -> "pred(" ^ string_of_term t' ^ ")"
      | TmAbs (x, typ, body) -> "(\\" ^ x ^ ": " ^ string_of_typ typ ^ "." ^ string_of_term body ^ ")"
      | TmApp (t1, t2) -> string_of_term t1 ^ " " ^ string_of_term t2
      | TmFree x -> x
      | TmBound (x, i) -> x ^ string_of_int i
      | TmLet (name, typ, t', body) -> "let " ^ name ^ " as " ^ string_of_typ typ ^ " = " ^ string_of_term t' ^ " in " ^ string_of_term body

  let rec desugar (t: term): term =
    match t with
    | TmLet(name, typ, t', body) -> TmApp (TmAbs (name, typ, desugar body), desugar t')
    | _ -> t
end


module StringMap = Map.Make(String)
let rec tag_debruijn' (t: Named.term) (m: int StringMap.t): Debruijn.term =
  match t with
  | TmZero -> TmZero
  | TmSucc t' -> TmSucc (tag_debruijn' t' m)
  | TmPred t' -> TmPred (tag_debruijn' t' m)
  | TmApp (a, b) -> TmApp (tag_debruijn' a m, tag_debruijn' b m)
  | TmVar x -> begin
      match StringMap.find_opt x m with
      | Some i -> TmBound (x, i)
      | None -> TmFree x
  end
  | TmAbs (name, ty, t') -> 
          let m' = m
            |> StringMap.map (fun i -> i + 1)
            |> StringMap.add name 1 in
          TmAbs (name, ty, tag_debruijn' t' m')
  | TmLet (name, typ, t', body) ->
          let m' = m
            |> StringMap.map (fun i -> i + 1)
            |> StringMap.add name 1 in
            TmLet (name, typ, tag_debruijn t', tag_debruijn' body m')
and tag_debruijn t = tag_debruijn' t StringMap.empty

let%expect_test "tag_debruijn" =
  let run x = x
      |> tag_debruijn
      |> Debruijn.string_of_term
      |> print_string in
  run (TmAbs ("x", Nat, TmAbs("y", Nat, TmVar("z"))));
  [%expect "(\\x: Nat.(\\y: Nat.z))"];
  run (TmAbs ("x", Nat, TmAbs("y", Nat, TmVar("y"))));
  [%expect "(\\x: Nat.(\\y: Nat.y1))"];
  run (TmAbs ("x", Nat, TmAbs("y", Nat, TmVar("x"))));
  [%expect "(\\x: Nat.(\\y: Nat.x2))"];
  run (TmAbs ("x", Nat, TmAbs("x", Nat, TmVar("x"))));
  [%expect "(\\x: Nat.(\\x: Nat.x1))"];
  run (TmLet ("x", Nat, TmZero, TmSucc(TmVar("x"))));
  [%expect "let x as Nat = 0 in succ x"];
