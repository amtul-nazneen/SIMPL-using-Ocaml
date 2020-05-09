open Simpltypes;;

type vartyp =
        Undeclared
      | VTyp of (ityp * bool)

type typctx = varname -> vartyp

type cmdtyp = TypCtx of typctx | CTypErr of string

type exprtyp = ExpTyp of ityp | ETypErr of string

let update s v i = (fun x -> if x=v then i else (s x));;

let init_typctx (l : (varname*vartyp) list) : typctx =
  fun x -> (try (List.assoc x l) with Not_found -> Undeclared);;

let rec typchk_expr (tc:typctx) (e:iexpr) : exprtyp =
  (* YOUR CODE GOES HERE
   * First copy your assignment 5 solution for typchk_expr into this space
   * (or copy the solution from the solution set to assignment 5).  Then add
   * two new cases to your match statement: one for Abstraction and
   * one for Apply.  See the simpltypes.ml file for definitions of these two
   * type-constructors.  See the assignment handout for the typing rules
   * that govern how abstractions and applications should be type-checked. *)
  ETypErr "not yet implemented"  (* DELETE THIS LINE *)

and typchk_cmd (tc:typctx) (c:icmd) : cmdtyp =
  (* YOUR ASSIGNMENT 5 SOLUTION GOES HERE
   * Copy your assignment 5 solution for typchk_cmd into this space (or copy
   * the solution set if your solution didn't work).  This part is unchanged
   * from assignment 5. *)
  CTypErr "not yet implemented";;  (* DELETE THIS LINE *)


(* Your interpreter may throw the SegFault exception if it ever encounters
 * a stuck state. *)
exception SegFault

(* Stores now map variable names to either integers or code. Code consists
 * of a command and a list of the names of the variables it takes as input. *)
type heapval = Data of int | Code of (varname list * icmd)
type store = varname -> heapval

let init_store (l : (varname*heapval) list) : store =
  fun x -> List.assoc x l;;

let rec eval_expr (s:store) (e:iexpr) : heapval =

  let eval_intop f (e1,e2,_) =
    (match (eval_expr s e1, eval_expr s e2) with
       (Data n1, Data n2) -> Data (f n1 n2)
     | _ -> raise SegFault) in

  let eval_boolop f =
    eval_intop (fun x y -> if (f (x<>0) (y<>0)) then 1 else 0) in

  (match e with
     Const (n,_) -> Data n
   | Var (x,_) -> (s x)
   | Plus z -> eval_intop (+) z
   | Minus z -> eval_intop (-) z
   | Times z -> eval_intop ( * ) z
   | True _ -> Data 1
   | False _ -> Data 0
   | Leq z -> eval_intop (fun x y -> if x<=y then 1 else 0) z
   | Conj z -> eval_boolop (&&) z
   | Disj z -> eval_boolop (||) z
   | Neg (e1,li) -> eval_boolop (fun x _ -> not x) (e1,True li,li)
   | Abstraction (al,c,_) ->
       (* YOUR CODE GOES HERE
        * Replace the following line with an implementation of the
        * large-step operational semantics for function abstraction. *)
       raise SegFault (* DELETE THIS LINE *)
   | Apply (e1,el,_) ->
       (* YOUR CODE GOES HERE
        * Replace the following line with an implementation of the
        * large-step operational semantics for function application. *)
       raise SegFault (* DELETE THIS LINE *)
  )

and exec_cmd (s:store) (c:icmd) : store =
  (match c with
     Skip _ | Decl _ -> s
   | Seq (c1,c2,_) -> exec_cmd (exec_cmd s c1) c2
   | Assign (v,e,_) -> update s v (eval_expr s e)
   | Cond (e,c1,c2,_) ->
       exec_cmd s (if (eval_expr s e)=(Data 0) then c2 else c1)
   | While (e,c1,li) -> exec_cmd s (Cond (e,Seq (c1,c,li),Skip li,li))
  );;



let main () =
   let argval = (function "true" -> 1 | "false" -> 0 | x -> int_of_string x) in
   let argtyp = (function "true" | "false" -> TypBool | _ -> TypInt) in
   let c = (Simplparser.parse_cmd Simpllexer.token 
              (Lexing.from_channel (open_in Sys.argv.(1)))) in
   let s = init_store (List.tl (List.tl (Array.to_list (Array.mapi
             (fun i a -> ("arg"^(string_of_int (i-2)),
                          Data (if i>=2 then (argval a) else 0)))
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
         | VTyp(rtyp,true) ->
             (match (exec_cmd s c "ret") with
                Code _ -> "<code>"
              | Data n -> if rtyp=TypInt then (string_of_int n)
                          else if n=0 then "false" else "true"));
        print_newline ()));;

main ();;

