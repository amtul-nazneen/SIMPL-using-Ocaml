open Simpltypes;;

(* A SIMPL variable's type is either Undeclared (it is an error to use it in
 * this case) or it has a declared type of "ityp" (see imptypes.ml for a
 * definition of ityp). The bool part should be true if the variable has been
 * initialized (i.e., it has been assigned a value) or false if it is still
 * uninitialized. *)
type vartyp =
        Undeclared
      | VTyp of (ityp * bool)

(* A typing context maps variable names to variable types *)
type typctx = varname -> vartyp

(* A SIMPL command's "type" is the typing context that results after it is
 * executed (or CTypErr if it is not well-typed). *)
type cmdtyp = TypCtx of typctx | CTypErr of string

(* A SIMPL expression's type is an ityp (or ETypErr if it is not well-typed). *)
type exprtyp = ExpTyp of ityp | ETypErr of string

(* Here is our old friend the 'update' function. Note that 'update' can
 * be used to update typing contexts as well as stores because it is a
 * polymorphic function. That is, update has the following generic type:
 *    ('a -> 'b) -> 'a -> 'b -> ('a -> 'b)
 * When given a store as its first argument, 'a = varname and 'b = int.
 * When given a typctx as its first argument, 'a = varname and 'b = vartyp.
 * Thus, (update tc v vt) returns a new store that adds mapping v->vt
 * to typing context tc. No need to write new code for this! *)
let update s v i = (fun x -> if x=v then i else (s x));;

let init_typctx (l : (varname*vartyp) list) : typctx =
  (* YOUR CODE GOES HERE
   * Replace the following line with code that takes a list of
   * (varname,vartyp) pairs and returns a typing context function that maps
   * each varname to each corresponding vartype. If the typing context
   * function you return is applied to a variable not in the list, it must
   * return the value Undeclared; it must NOT throw an exception. *)
  raise (Failure "not yet implemented");;   (* <-- delete this line *)

let rec typchk_expr (tc:typctx) (e:iexpr) : exprtyp =
  (* YOUR CODE GOES HERE
   * Replace the following line with code to typecheck an iexpr. If iexpr is
   * well-typed, then you should return (ExpTyp t) where t is the expression's
   * type. Otherwise you should return (ETypErr s) where s is an error message
   * string.  In your solution, you should consider use of an uninitialized
   * variable to be a typing error. *)
  ETypErr "not yet implemented";;   (* <-- delete this line *)

let rec typchk_cmd (tc:typctx) (c:icmd) : cmdtyp =
  (* YOUR CODE GOES HERE
   * Replace the following line with code to typecheck an icmd. If icmd is
   * well-typed, then you should return (TypCtx tc) where tc is a typing
   * context that maps the names of all the program's variables to their
   * declared types. If it is not well-typed, you should return (CTypErr s).
   * The initial typing context passed to this function will map argument
   * variables to VTyp (TypInt,true) and everything else to Undeclared. *)
  CTypErr "not yet implemented";;   (* <-- delete this line *)


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

