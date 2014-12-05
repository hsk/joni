%{
open Ast

let addBlock = function
  | (SBlock _ as b) -> b
  | b -> SBlock [b]

%}

%token <string> PACKAGE
%token <string> IMPORT
%token <string> STRING
%token <int> INT
%token <string> ID

%token STATIC PUBLIC PRIVATE PROTECTED FINAL
%token CLASS THIS TRAIT EXTENDS REXTENDS
%token IF ELSE
%token MATCH OR ARROW
%token RETURN
%token COLON COMMA SEMICOLON

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token HAT SHARP HART
%token ASSIGN
%token CAST
%token EQ NE
%token LT GT LE GE
%token SUB ADD
%token MUL DIV
%token NEW
%token DOT
%token AT

%token EOF

/* operator precedence */

%right ASSIGN
%right CAST
%left EQ NE
%left LT GT LE GE
%left ADD SUB
%left MUL DIV
%left DOT
%left AT
%left NEW
%left prec_app

%type <Ast.prog> prog
%start prog

%%

prog:
  | defs { Prog($1) }

typ:
  | ID { Ty $1 }
  | ID LBRACK typ RBRACK %prec prec_app{ TGen($1, $3) }

typs:
  | typ { [$1] }
  | typ COMMA typs { $1 :: $3 }

simple_exp:
  | LPAREN exp RPAREN { $2 }
  | INT { EInt($1) }
  | STRING { EString($1) }
  | ID { EVar($1) }

exps:
  | exp { [$1] }
  | exp COMMA exps { $1 :: $3 }

exp2:
  | exp { $1 }
  | SEMICOLON exp { $2 }

exp:
  | simple_exp { $1 }
  | exp ASSIGN exp2 { EBin($1, "=", $3) }

  | exp EQ exp2 { EBin($1, "==", $3) }
  | exp NE exp2 { EBin($1, "!=", $3) }

  | exp LT exp2 { EBin($1, "<", $3) }
  | exp GT exp2 { EBin($1, ">", $3) }
  | exp LE exp2 { EBin($1, "<=", $3) }
  | exp GE exp2 { EBin($1, ">=", $3) }

  | exp ADD exp2 { EBin($1, "+", $3) }
  | exp SUB exp2 { EBin($1, "-", $3) }

  | exp MUL exp2 { EBin($1, "*", $3) }
  | exp DIV exp2 { EBin($1, "/", $3) }

  | exp DOT exp2 { EBin($1, ".", $3) }

  | NEW exp2 { EPre("new", $2) }
  | SUB exp2 %prec NEW { EPre("-", $2)}

  | AT exp2 { EBin(EVar("this"), ".", $2) }
  | exp CAST typ { ECast($3, $1) }
  | ID LPAREN RPAREN { ECall(EVar($1), []) }
  | ID LPAREN exps RPAREN { ECall(EVar($1), $3) }
  | ID LPAREN exps COMMA RPAREN { ECall(EVar($1), $3) }

ids:
  | ID { [$1] }
  | ID COMMA ids { $1::$3 }

cases:
  | case { [$1] }
  | case cases { $1::$2 }
case:
  | OR ID ARROW stmts { ($2, $4) }

stmts:
  | stmt { [$1] }
  | stmt stmts { $1 :: $2 }

stmt:
  | exp { SExp($1) }
  | RETURN exp { SRet($2) }
  | RETURN { SRet(EEmpty) }
  | LBRACE RBRACE { SBlock([]) }
  | LBRACE stmts RBRACE { SBlock($2) }
  | IF LPAREN exp RPAREN stmt { SIf($3, $5, SEmpty) }
  | IF LPAREN exp RPAREN stmt ELSE stmt { SIf($3, $5, $7) }
  | ID COLON ID { SLet(Ty $3, EVar $1, EEmpty) }
  | ID COLON typ ASSIGN exp { SLet($3, EVar $1, $5) }
  | exp MATCH LBRACE cases RBRACE { SMatch($1, $4) }

defs:
  | adef { [$1] }
  | adef defs { $1 :: $2 }
  | adef defs { $1 :: $2 }

access:
  | HAT { AStatic }
  | HART { AStatic }
  | ADD { APublic }
  | SUB { APrivate }
  | SHARP { AProtected }
  | FINAL { AFinal }

accesses:
  | access { [$1] }
  | access accesses { $1::$2 }

adef:
  | accesses def { SAccess($1, $2) }
  | def { $1 }
  | adef SEMICOLON { $1 }
def:
  | PACKAGE { SPackage($1) }

  | ID LPAREN RPAREN COLON ID ASSIGN stmt %prec prec_app {

      SFun(Ty $5, $1, [], addBlock $7)
    }
  | ID LPAREN prms RPAREN COLON ID ASSIGN stmt %prec prec_app {

      SFun(Ty $6, $1, $3, addBlock $8)
    }
  | ID COLON ID { SLet(Ty $3, EVar $1, EEmpty) }
  | ID COLON typ ASSIGN exp { SLet($3, EVar $1, $5) }
  | class_ { $1 }
  | ID TRAIT LBRACE trait_defs RBRACE { STrait($1, $4) }

extends:
  | ID { ($1, "") }
  | ID EXTENDS  ID { ($3, $1) }
  | ID REXTENDS ID { ($1, $3) }
class_:
  | extends CLASS LBRACE RBRACE { SClass(fst $1, snd $1, []) }
  | extends CLASS LBRACE defs RBRACE { SClass(fst $1, snd $1, $4) }
  | extends CLASS LPAREN RPAREN {
      SClass(fst $1, snd $1,[SCon(fst $1, [],SBlock [])])
    }

  | extends CLASS LPAREN prms RPAREN {
      let mems = List.map begin fun (ty, id) ->
        SLet(ty, EVar id, EEmpty)
      end $4 in
      let inits = List.map begin fun (ty, id) ->
        SExp(EBin(EBin(EVar "this", ".", EVar id), "=", EVar id))
      end $4 in
      SClass(fst $1, snd $1, SCon(fst $1, $4,SBlock inits)::mems)
    }

  | extends CLASS LPAREN prms RPAREN LBRACE RBRACE {
      let mems = List.map begin fun (ty, id) ->
        SLet(ty, EVar id, EEmpty)
      end $4 in
      let inits = List.map begin fun (ty, id) ->
        SExp(EBin(EBin(EVar "this", ".", EVar id), "=", EVar id))
      end $4 in
      SClass(fst $1, snd $1, SCon(fst $1, $4, SBlock inits)::mems)
    }

  | extends CLASS LPAREN prms RPAREN LBRACE defs RBRACE {
      let mems = List.map begin fun (ty, id) ->
        SLet(ty, EVar id, EEmpty)
      end $4 in
      let inits = List.map begin fun (ty, id) ->
        SExp(EBin(EBin(EVar "this", ".", EVar id), "=", EVar id))
      end $4 in
      SClass(fst $1, snd $1, SCon(fst $1, $4, SBlock inits)::mems @ $7)
    }


trait_defs:
  | trait_def { [$1] }
  | trait_def trait_defs { $1 :: $2 }

trait_def:
  | ID LPAREN RPAREN COLON ID {
      SFun(Ty $5, $1, [], SEmpty)
    }
  | ID LPAREN prms RPAREN COLON ID {
      SFun(Ty $6, $1, $3, SEmpty)
    }
prms:
  | prms_ {
    let(_,ls) = List.fold_left (fun (n,ls) m  ->
      match m with
      | (t,"") -> (n+1,(t,("_"^string_of_int n))::ls)
      | (t,id) -> (n+1,(t,id)::ls)
    ) (1, []) $1
    in List.rev ls
  }

prms_:
  | ID COLON typ { [$3, $1] }
  | ID COLON typ COMMA prms_ { ($3, $1)::$5 }
  | typ { [$1,""] }
  | typ COMMA prms_ { ($1,"")::$3 }

