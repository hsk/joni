# GomaJ Programming Language

GomaJ(beta) is tiny AltJ, namely, GomaJ is a very simple limited translator for the Java programming language.

This compiler made by OCaml, OCamlYacc and OCamlLex.

## build

    $ make

## hello world

#### example/Hello.gomaj

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

    $ ./gomajc example/hello.gomaj example/Hello.java
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

