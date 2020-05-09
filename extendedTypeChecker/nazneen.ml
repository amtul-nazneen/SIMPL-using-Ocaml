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

let li2str ((l1,c1),(l2,c2)) =
    "Pos "^(string_of_int l1)^":"^(string_of_int c1)^"-"^
    (string_of_int l2)^":"^(string_of_int c2)^": ";;

let eerr li s = ETypErr ((li2str li)^s);;
let cerr li s = CTypErr ((li2str li)^s);;

let rec itypToString (i:ityp) : string =
  let list_to_string (l: ityp list) : string =
    List.fold_left (fun acc x -> acc ^ (itypToString x)) "" l in
  (match i with
      | TypInt -> "TypInt"
      | TypBool -> "TypBool"
      | TypFunc (l, typ) -> "TypFunc(" ^ (list_to_string l) ^ ", " ^ (itypToString typ) ^ ") "
  )

(* Helper function *)
let notExistsImage (l:iformal list) (tc:typctx) : bool =
  List.fold_left (fun acc (v, typ, li) ->
    (match (tc v) with
      | Undeclared -> true && acc
      | VTyp (_, _) -> false
    )
  )  true l;;

(* Helper function *)
let updateTypCtx (l:iformal list) (tc:typctx) : typctx =
  List.fold_left (fun acc (v, typ, li) -> update acc v (VTyp (typ,true))) tc l;;

(* Helper function *)
let typList (l:iformal list) : (ityp list) =
  List.fold_left (fun acc (_, typ, _) -> typ::acc) [] (List.rev l);;

(* Begin of typchk_expr *)
let rec typchk_expr (tc:typctx) (e:iexpr) : exprtyp =

(* Helper function *)
let rec compareList (el:iexpr list) (tl:ityp list) (tc:typctx) : (bool * exprtyp)  =
    List.fold_left2 (fun (acc, x) exp typ ->
      (match (typchk_expr tc exp) with
        | ExpTyp t -> if (t <> typ) then (false, ETypErr (  "type-mismatch") ) else (acc && true, x)
        | ETypErr t -> (false, ETypErr t)
      )
    ) (true, ETypErr "null") el tl in


let typchk_op name argtyp rettyp (e1,e2,li) : exprtyp =
    (match (typchk_expr tc e1, typchk_expr tc e2) with
        (ETypErr s,_) | (_,ETypErr s) -> ETypErr s
      | (ExpTyp t1, ExpTyp t2) ->
          if t1=argtyp && t2=argtyp then (ExpTyp rettyp)
          else eerr li ("type-mismatch in argument to "^name)
    ) in
(match e with
  Const _ -> ExpTyp TypInt
| Var (v,li) ->
    (match (tc v) with VTyp (t,true) -> ExpTyp t
    | VTyp (_,false) -> eerr li ("variable "^v^" used uninitialized")
    | Undeclared -> eerr li ("variable "^v^" used undeclared"))
| Plus x -> typchk_op "+" TypInt TypInt x
| Minus x -> typchk_op "-" TypInt TypInt x
| Times x -> typchk_op "*" TypInt TypInt x
| True _ | False _ -> ExpTyp TypBool
| Leq x -> typchk_op "<=" TypInt TypBool x
| Conj x -> typchk_op "&&" TypBool TypBool x
| Disj x -> typchk_op "||" TypBool TypBool x
| Neg (e1,li) -> typchk_op "!" TypBool TypBool (e1,True li,li)
| Abstraction (li,c,lineinfo) ->
  let verify = (notExistsImage li tc) in
  (match verify with
      | true -> let newTyp= (updateTypCtx li tc) in
                let tcprime = (typchk_cmd newTyp c) in
                    (match tcprime with
                       TypCtx tp ->
                       (match (tp "ret") with
                                      | Undeclared -> eerr lineinfo ("variable ret undeclared")
                                      | VTyp (finaltyp,true) -> let tl = (typList li) in ExpTyp (TypFunc (tl,finaltyp))
                                      | VTyp (_,false) -> eerr lineinfo ("variable ret uninitialized")
                       )
                     | CTypErr x -> ETypErr x
                    )
      | false -> eerr lineinfo ("variable already declared")
  )
| Apply (e,e1list,lineinfo) ->
    (match (typchk_expr tc e) with
        | ExpTyp TypInt -> eerr lineinfo ("TypeFunc expected, got TypInt")
        | ExpTyp TypBool -> eerr lineinfo ("TypeFunc expected, got TypBool")
        | ExpTyp  TypFunc (typlist,returntype) ->
                 if (List.compare_lengths e1list typlist <> 0)
                    then eerr lineinfo ("ExprList and FuncList length mismatch")
                 else(
                     let (bresult,err) =  (compareList e1list typlist tc) in
                        if bresult=true
                            then ExpTyp returntype
                        else err
                    )
        | ETypErr x -> ETypErr x
    )
)
and  typchk_cmd (tc:typctx) (c:icmd) : cmdtyp =
(match c with
Skip _ -> TypCtx tc
| Seq (c1,c2,_) -> (match (typchk_cmd tc c1) with
CTypErr s -> CTypErr s
| TypCtx tc2 -> (typchk_cmd tc2 c2))
| Assign (v,e1,li) ->
(match (tc v, typchk_expr tc e1) with
(_, ETypErr s) -> CTypErr s
| (VTyp (t1,_), ExpTyp t2) ->
if t1=t2 then TypCtx (update tc v (VTyp (t1,true)))
else cerr li ("type-mismatch in assignment to "^v)
| (Undeclared, _) -> cerr li ("assignment to undeclared var "^v))
| Cond (e1,c1,c2,li) ->
(match (typchk_expr tc e1, typchk_cmd tc c1, typchk_cmd tc c2) with
(ETypErr s,_,_) | (_,CTypErr s,_) | (_,_,CTypErr s) -> CTypErr s
| (ExpTyp TypBool, TypCtx _, TypCtx _) -> TypCtx tc
| (ExpTyp _, TypCtx _, TypCtx _) -> cerr li "if test is not a bool")
| While (e1,c1,li) ->
(match (typchk_expr tc e1, typchk_cmd tc c1) with
(ETypErr s, _) | (_, CTypErr s) -> CTypErr s
| (ExpTyp TypBool, TypCtx _) -> TypCtx tc
| (ExpTyp _, TypCtx _) -> cerr li "while test is not a bool")
| Decl (t,v,li) -> (match (tc v) with
Undeclared -> TypCtx (update tc v (VTyp (t,false)))
| _ -> cerr li ("name conflict: "^v)));;

(* Your interpreter may throw the SegFault exception if it ever encounters
 * a stuck state. *)
exception SegFault

(* Stores now map variable names to either integers or code. Code consists
 * of a command and a list of the names of the variables it takes as input. *)
type heapval = Data of int | Code of (varname list * icmd)
type store = varname -> heapval

let init_store (l : (varname*heapval) list) : store =
  fun x -> List.assoc x l;;

(* Helper Functions *)
let makeCode (al:iformal list) (c:icmd) : heapval =
  let varList = List.fold_left (fun acc (v, _, _) -> v::acc) [] (List.rev al) in
  Code (varList, c);;

(* Helper Functions *)
let updateStoreList (v: varname list) (h: heapval list) (s:store) : store =
  if List.compare_lengths v h <> 0 then raise SegFault
  else  List.fold_left2 (fun acc var heap -> update acc var heap) s v h;;


(* Begin of eval_expr *)
let rec eval_expr (s:store) (e:iexpr) : heapval =

  let eval_intop f (e1,e2,_) =
    (match (eval_expr s e1, eval_expr s e2) with
       (Data n1, Data n2) -> Data (f n1 n2)
     | _ -> raise SegFault) in

  let eval_boolop f =
    eval_intop (fun x y -> if (f (x<>0) (y<>0)) then 1 else 0) in

   (* Helper Functions*)
  let evalList (el: iexpr list) (s:store) : (heapval list) =
    List.fold_left (fun acc x -> (eval_expr s x)::acc) [] (List.rev el) in

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
   | Abstraction (al,c,_) -> makeCode al c
   | Apply (e1,el,_) ->
      (match (eval_expr s e1) with
            | Code (varList,c) ->
                let heapList = (evalList el s) in
                let updatedStore = (updateStoreList varList heapList s) in
                let storePrime = (exec_cmd updatedStore c) in
                storePrime "ret"
            | Data _ ->  raise SegFault)
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
