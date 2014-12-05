let trans input output =

  let inp = open_in input in
  let lexbuf = Lexing.from_channel inp in
  let ast = Parser.prog Lexer.token lexbuf in
  close_in inp;

  let out = open_out output in
  Gen_java.print_prog out ast;
  close_out out

let gomaj2java src =
  let len = String.length src in
  if String.sub src (len - 6) 6 = ".gomaj"
  then
    String.sub src 0 (len - 6) ^ ".java"
  else
    failwith "filename is bad."

let _ =
  let gomaj = Sys.argv.(1) in
  let java = gomaj2java(gomaj) in
  trans gomaj java
