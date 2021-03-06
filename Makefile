all:
	cd src; ocamlyacc parser.mly
	rm src/parser.mli
	cd src; ocamllex lexer.mll
	cd src; ocamlc ast.ml parser.ml lexer.ml gen_java.ml main.ml -o ../jonic

opt:
	cd src; ocamlyacc parser.mly
	rm src/parser.mli
	cd src; ocamllex lexer.mll
	cd src; ocamlopt ast.ml parser.ml lexer.ml gen_java.ml main.ml -o ../jonic

hello:
	./jonic example/Hello.joni
	javac example/Hello.java
	java example.Hello

fib:
	./jonic example/Fib.joni
	javac example/Fib.java
	java example.Fib

test: example/Test.joni
	./jonic example/Test.joni
	javac example/Test.java
	java example.Test

clean:
	rm -rf jonic jonic.opt example/*.java example/*.class src/.omakedb src/*.cm* src/parser.ml src/lexer.ml src/*.o
