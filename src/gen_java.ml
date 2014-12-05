open Printf
open Ast

let sp = ref ""

let block print_fun =
  let old_indent_space = !sp in
  sp := !sp ^ "  ";
  print_fun ();
  sp := old_indent_space

module M = Map.Make (String)

let infixs =
  List.fold_left (fun m (k,prec,left) -> M.add k (prec,left) m) M.empty
    [
      "=",  1, false;
      "==", 2, true;
      "!=", 2, true;
      "<",  3, true;
      ">",  3, true;
      "<=", 4, true;
      ">=", 5, true;
      "+",  6, true;
      "-",  6, true;

      "/",  7, true;
      "*",  7, true
    ]

let prefixs =
  List.fold_left (fun m (k,prec,ident) -> M.add k (prec,ident) m ) M.empty
    [
      "new", 8, true;
      "!",   8, false;
      "-",   8, false
    ]

let postfixs =
  List.fold_left (fun m (k,prec,ident) -> M.add k (prec,ident) m ) M.empty
    [
      "++", 9, false;
      "--", 9, false
    ]

let fp = ref stdout

let rec print_ls sep print_fun = function
  | [] -> ()
  | [x] -> print_fun x
  | x :: xs ->
    print_fun x;
    fprintf !fp "%s" sep;
    print_ls sep print_fun xs

let rec print_iter print_fun sep = 
  List.iter (fun a ->
    print_fun a;
    fprintf !fp "%s" sep
  )

let rec print_t = function

  | Ty(s) -> fprintf !fp "%s" s

  | TGen(s, t) ->
    begin match s with
      | "Array" ->
        print_t t;
        fprintf !fp "[]"
      | _ ->
        fprintf !fp "%s<" s;
        print_t t;
        fprintf !fp ">"
    end

let rec print_e ?(paren=true) ?(p=0) = function 
  | EEmpty -> ()

  | EInt i -> fprintf !fp "%d" i

  | EVar i -> fprintf !fp "%s" i

  | EString i -> fprintf !fp "%s" i

  | EPre(op, e1) ->

    let (p1,ident) = (M.find op prefixs) in
    let paren = paren && p1 < p in

    if paren then fprintf !fp "(";
    if ident then fprintf !fp " %s " op
             else fprintf !fp " %s"  op;
    print_e e1 ~p:p1;
    if paren then fprintf !fp ")"

  | EPost(e1, op) ->

    let (p1,ident) = (M.find op postfixs) in
    let paren = paren && p1 <= p in

    if paren then fprintf !fp "(";
    if ident then fprintf !fp " %s " op
             else fprintf !fp " %s"  op;
    print_e e1 ~p:(p1-1);
    if paren then fprintf !fp ")"

  | EBin(e1, ".", e2) ->
    print_e e1;
    fprintf !fp ".";
    print_e e2

  | EBin(e1, op, e2) ->
    let (p1,l) = (M.find op infixs) in
    let paren = paren && (if l then p1 <= p else p1 < p) in
    if paren then fprintf !fp "(";
    print_e e1 ~p:(if l then p1 - 1 else p1 + 1);
    fprintf !fp "%s" op;
    print_e e2 ~p:p1;
    if paren then fprintf !fp ")"

  | ECall(e1, es) ->
    print_e e1;
    fprintf !fp "(";
    print_ls ", " (print_e ~paren:false) es;
    fprintf !fp ")"

  | EArr(e1, es) ->
    print_e e1;
    fprintf !fp "[";
    print_ls ", " (print_e ~paren:false) es;
    fprintf !fp "]";

  | ECast(t, e) ->
    fprintf !fp "(";
    print_t t;
    fprintf !fp ")";
    print_e e

let print_a = function
  | APublic    -> fprintf !fp "public"
  | APrivate   -> fprintf !fp "private"
  | AProtected -> fprintf !fp "protected"
  | AStatic    -> fprintf !fp "static"
  | AFinal     -> fprintf !fp "final"

let rec print_s ?(nest=true) (s:s):unit =

  if nest then
    fprintf !fp "%s" !sp;
  match s with

  | SAccess(acs, s) ->
    print_iter print_a " " acs;
    print_s ~nest:false s

  | SEmpty ->
    ()

  | SExp e ->
    print_e e ~paren:false;
    fprintf !fp ";"

  | SRet e ->
    fprintf !fp "return ";
    print_e e ~paren:false;
    fprintf !fp ";"

  | SPackage s ->
    fprintf !fp "package %s;" s

  | SBlock ss ->
    fprintf !fp "{\n";
    block begin fun()->
      print_iter print_s "\n" ss
    end;
    fprintf !fp "%s}" !sp

  | SLet (t, id, EEmpty) ->
    print_t t;
    fprintf !fp " ";
    print_e id ~paren:false;
    fprintf !fp ";"

  | SLet (t, id, e) ->
    print_t t;
    fprintf !fp " ";
    print_e id ~paren:false;
    fprintf !fp " = ";
    print_e e;
    fprintf !fp ";";

  | SCon(id, tis, e) ->
    fprintf !fp "%s(" id;
    print_ls ", " begin fun (t, i) ->
      print_t t;
      fprintf !fp " %s" i
    end tis;
    fprintf !fp ") ";
    print_s e ~nest:false

  | SFun (t, id, ts, s) ->
    let f (t, a) =
      print_t t;
      fprintf !fp " %s" a
    in
    print_t t;
    fprintf !fp " %s(" id;
    print_ls ", " f ts;
    fprintf !fp ") ";
    print_s s ~nest:false;

  | SIf(e1, e2, e3) ->

    let print_block ed = function

      | SBlock ls as e ->
        fprintf !fp " ";
        print_s e ~nest:false;
        if ed = ("\n"^ !sp) then fprintf !fp " ";

      | SIf(_, _, _) as e->
        fprintf !fp " ";
        print_s e ~nest:false;

      | e ->
        block begin fun () ->
          fprintf !fp "\n";
          print_s e;
          fprintf !fp "%s" ed
        end
    in
    fprintf !fp "if (";
    print_e e1 ~paren:false;
    fprintf !fp ")";
    begin match e3 with
      | SEmpty ->
        print_block "\n" e2;
        fprintf !fp "%s" !sp
      | _ ->
        print_block ("\n" ^ !sp) e2; 
        fprintf !fp "else";
        print_block "" e3
    end

  | SClass (id, super, ss) ->
    fprintf !fp "class %s" id;
    if super <> "" then
      fprintf !fp " extends %s" super;
    fprintf !fp " {\n";
    block begin fun() ->
      print_iter print_s "\n" ss
    end;
    fprintf !fp "%s}" !sp

  | STrait (id, ss) ->
    fprintf !fp "interface %s {\n" id;
    block begin fun()->
      print_iter print_s ";\n" ss
    end;
    fprintf !fp "\n%s}\n" !sp

  | SMatch (exp, ss) ->
    List.iter (fun (id, ss) ->
      fprintf !fp "\n%sif (" !sp;
      print_e exp;
      fprintf !fp " instanceof %s) {\n" id;
      block begin fun() ->
        fprintf !fp "%s%s $ = (%s)" !sp id id;
        print_e exp;
        fprintf !fp ";\n";
        print_iter print_s "\n" ss;
      end;
      fprintf !fp "%s}\n" !sp;

    ) ss

let print_prog ffp (Prog(ls)) =
  sp := "";
  fp := ffp;
  print_ls "\n" print_s ls;
  fp := stdout

