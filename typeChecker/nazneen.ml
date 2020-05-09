(*   Name: Amtul Nazneen (AXN180041)
   * Collaborator: Carla Vazquez (CPV150030)
   * Sources:
   *        - https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
   *        - https://ocaml.org/learn/tutorials/99problems.html
   *        - OCaml Transcripts from class, Lecture Notes *)

open Simpltypes;;

type vartyp = Undeclared| VTyp of (ityp * bool)
type typctx = varname -> vartyp
type cmdtyp = TypCtx of typctx | CTypErr of string
type exprtyp = ExpTyp of ityp | ETypErr of string

let update s v i = (fun x -> if x=v then i else (s x));;

(*Helper Function to Print LineInfo*)
let printline msg ((a,b),(c,d)) = msg^" at Line:"^string_of_int(a)^" ,Column:"^string_of_int(b)^
                                   ", Line:"^string_of_int(c)^" ,Column:"^string_of_int(d);;

(*Initialise the Typing Context*)
let init_typctx (l : (varname*vartyp) list) : typctx =
  fun x -> try (List.assoc x l) with _ -> Undeclared ;;

(*Type Check Expressions*)
let rec typchk_expr (tc:typctx) (e:iexpr) : exprtyp =
    match e with
   Const (x,l) -> (ExpTyp TypInt)
 | True l -> (ExpTyp TypBool)
 | False l -> (ExpTyp TypBool)
 | Var (v,l) ->
     (match tc v with
            Undeclared -> ETypErr (printline "Undeclared Variable" l)
          | VTyp(x,y) -> ExpTyp x)
 | Plus (x,y,l) ->
     (match typchk_expr tc x with
            ExpTyp TypInt ->
            (match typchk_expr tc y with
                   ExpTyp TypInt -> ExpTyp TypInt
                 | ETypErr err-> ETypErr err)
          | ETypErr err -> ETypErr err)
| Minus (x,y,l) ->
    (match typchk_expr tc x with
           ExpTyp TypInt ->
              (match typchk_expr tc y with
                     ExpTyp TypInt -> ExpTyp TypInt
                   | ETypErr err-> ETypErr err)
         | ETypErr err -> ETypErr err)
 | Times (x,y,l) ->
     (match typchk_expr tc x with
            ExpTyp TypInt ->
              (match typchk_expr tc y with
                     ExpTyp TypInt -> ExpTyp TypInt
                   | ETypErr err-> ETypErr err)
          | ETypErr err -> ETypErr err)
| Leq (x,y,l) ->
     (match typchk_expr tc x with
           ExpTyp TypInt ->
              (match typchk_expr tc y with
                     ExpTyp TypInt -> ExpTyp TypBool
                   | ETypErr err-> ETypErr err)
         | ETypErr err -> ETypErr err)
| Conj (x,y,l) ->
    (match typchk_expr tc x with
           ExpTyp TypBool ->
              (match typchk_expr tc y with
                     ExpTyp TypBool -> ExpTyp TypBool
                   | ETypErr err-> ETypErr err)
         | ETypErr err -> ETypErr err)
| Disj (x,y,l) ->
    (match typchk_expr tc x with
          ExpTyp TypBool ->
              (match typchk_expr tc y with
                     ExpTyp TypBool -> ExpTyp TypBool
                   | ETypErr err-> ETypErr err)
        | ETypErr err -> ETypErr err)
| Neg (x,l) ->
    (match typchk_expr tc x with
           ExpTyp TypBool -> ExpTyp TypBool
         | ETypErr err -> ETypErr err);;

(*Type Check Commands*)
let rec typchk_cmd (tc:typctx) (c:icmd) : cmdtyp =
    match c with
          Skip l -> TypCtx tc
        | Seq (x,y,l) ->
            (match (typchk_cmd tc x) with
                   CTypErr err -> CTypErr err
                 | TypCtx t2 ->
                      ( match (typchk_cmd t2 y) with
                              CTypErr err -> CTypErr err
                            | TypCtx t3 -> TypCtx t3))
        | Cond (e,c1,c2,l) ->
            (match (typchk_expr tc e) with ETypErr err1 -> CTypErr err1
                  | ExpTyp t ->
                    (match t with TypBool ->
                        (match (typchk_cmd tc c1) with CTypErr err -> CTypErr err
                          | _ ->
                          (
                             match (typchk_cmd tc c2) with CTypErr err -> CTypErr err
                             | _ -> TypCtx tc
                          ))
                     | _ -> CTypErr (printline "Not a boolean condition" l)
                    )
            )
        | While (e,c,l) ->
            (
                match (typchk_expr tc e) with ETypErr err -> CTypErr err
                | ExpTyp t ->
                ( match t with TypBool ->
                    (
                      match(typchk_cmd tc c) with CTypErr err -> CTypErr err
                      | _ -> TypCtx tc
                    )
                | _ -> CTypErr (printline "Not a boolean condition" l)
                )
            )
        | Decl (it,v,l) ->
            (
             match (tc v) with Undeclared -> TypCtx ( update tc v (VTyp(it,false)))
             | _ -> CTypErr (printline "Variable already declared" l)
            )
        | Assign (v,e,l) ->
            (
             match (typchk_expr tc e) with
                  ETypErr err1 -> CTypErr err1
                | ExpTyp t ->
                ( match (tc v) with
                  Undeclared -> CTypErr (printline "Undeclared Variable" l)
                | VTyp (x,y) ->
                            (if x=t then TypCtx ( update tc v (VTyp(x,true)))
                            else CTypErr (printline "Mismatch in type" l) )
                )
            );;


(* NO CHANGES REQUIRED AFTER THIS POINT
 * The following code is essentially the same as the interpreter you wrote
 * for assignment 2, except modified to interpret typed programs instead of
 * untyped programs. *)

type store = varname -> int

let init_store (l : (varname*int) list) : store =
  fun x -> List.assoc x l;;

let rec eval_expr (s:store) (e:iexpr) : int =
  (match e with
     Const (n,_) -> n
   | Var (x,_) -> (s x)
   | Plus (e1,e2,_)  | Disj (e1,e2,_) -> (eval_expr s e1) + (eval_expr s e2)
   | Minus (e1,e2,_) -> (eval_expr s e1) - (eval_expr s e2)
   | Times (e1,e2,_) | Conj (e1,e2,_) -> (eval_expr s e1) * (eval_expr s e2)
   | True _ -> 1
   | False _ -> 0
   | Leq (e1,e2,_) -> if (eval_expr s e1) <= (eval_expr s e2) then 1 else 0
   | Neg (e1,_) -> if (eval_expr s e1)=0 then 1 else 0
  );;

let rec exec_cmd (s:store) (c:icmd) : store =
  (match c with
     Skip _ | Decl _ -> s
   | Seq (c1,c2,_) -> exec_cmd (exec_cmd s c1) c2
   | Assign (v,e,_) -> update s v (eval_expr s e)
   | Cond (e,c1,c2,_) -> exec_cmd s (if (eval_expr s e)=0 then c2 else c1)
   | While (e,c1,li) -> exec_cmd s (Cond (e,Seq (c1,c,li),Skip li,li))
  );;


(* The main function now calls typchk_cmd (your code) before attempting to
 * execute the SIMPL program. Only well-typed SIMPL programs are executed. *)
let main () =
   let argval = (function "true" -> 1 | "false" -> 0 | x -> int_of_string x) in
   let argtyp = (function "true" | "false" -> TypBool | _ -> TypInt) in
   let c = (Simplparser.parse_cmd Simpllexer.token
              (Lexing.from_channel (open_in Sys.argv.(1)))) in
   let s = init_store (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)),
                          if i>=2 then (argval a) else 0))
             Sys.argv)))) in
   let tc = init_typctx (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)), VTyp (argtyp a,true)))
             Sys.argv)))) in
   (match (typchk_cmd tc c) with
      CTypErr s -> print_string ("Typing error: "^s^"\n")
    | TypCtx tc' -> (print_string
        (match (tc' "ret") with
           Undeclared -> "Typing error: return value undeclared"
         | VTyp(_,false) -> "Typing error: return value uninitialized"
         | VTyp(rtyp,true) -> let n = exec_cmd s c "ret" in
             if rtyp=TypInt then (string_of_int n)
             else if n=0 then "false" else "true");
        print_newline ()));;

main ();;
