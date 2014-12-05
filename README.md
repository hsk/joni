# Joni Programming Language

Joni(beta) is tiny AltJ, namely, joni is a very simple limited translator for the Java programming language.

This compiler made by OCaml, OCamlYacc and OCamlLex.

## build

    $ make

## hello world

#### example/Hello.joni

```
package example
Hello class {
  + ^ main():void = {
    System.out.println("hello world")
  }
}
```

`+` is public and `^` is static

#### build & run

    $ ./jonic example/hello.joni example/Hello.java
    $ javac example/Hello.java
    $ ./java example.Hello
    hello world!

or

    $ make hello

#### example/Hello.java

```
package example;
class Hello {
  public static void main() {
    System.out.println("hello world!");
  }
}
```

## examples

    $ make fib
    $ make test

## clean

    $ make clean

## License

MIT Licence.

