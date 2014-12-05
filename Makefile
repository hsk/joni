all:
	cd src; ocamlyacc parser.mly
	rm src/parser.mli
	cd src; ocamllex lexer.mll
	cd src; ocamlc ast.ml parser.ml lexer.ml gen_java.ml main.ml -o ../gomajc

opt:
	cd src; ocamlyacc parser.mly
	rm src/parser.mli
	cd src; ocamllex lexer.mll
	cd src; ocamlopt ast.ml parser.ml lexer.ml gen_java.ml main.ml -o ../gomajc

hello:
	./gomajc example/Hello.gomaj
	javac example/Hello.java
	java example.Hello

fib:
	./gomajc example/Fib.gomaj
	javac example/Fib.java
	java example.Fib

test: example/Test.gomaj
	./gomajc example/Test.gomaj
	javac example/Test.java
	java example.Test

clean:
	rm -rf gomajc gomajc.opt example/*.java example/*.class src/.omakedb src/*.cm* src/parser.ml src/lexer.ml src/*.o
