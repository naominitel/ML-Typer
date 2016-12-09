## Usage

### Prerequisites

```
opam install llvm
opam install batteries
```

The LLVM C library will probably have to be installed too.

### Building the compiler

```
ocamlbuild -use-ocamlfind -lflags -cc,g++ main.native
```

### Compiling and running a program

```
./main.native examples/test.ml
llc test.out
gcc test.out.s mlrt.c -o test
./test
```

You can also try the interactive toplevel:

```
./main.native
```

Note that this does not run the programs though. This just runs the typer and
displays the result.

### Syntax for programs

Mostly ML (no sum types), toplevel definitions use the keyword def. There is
no syntactic sugar for function definitions or expressions, which must be
explicitly curryfied.

The `fst` and `snd` functions access the head and the tail of a list.

```ocaml
def map =
    fun f -> fun l ->
        if l == [] then []
        else (f (fst l)) :: (map f (snd l))
>>>>>>> 40eb8e0... .
```
