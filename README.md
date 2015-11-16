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

### Language

###### Constants:

```
1       (* integers *)
()      (* unit *)
(1, 2)  (* tuples *)
[1 ; 2] (* lists *)
```

###### Function call:

```
f a b c d   (* (((f a) b) c) a *)
```

###### Functions:

```
fun x -> x
```

###### Conditionals:

```
if x = 0 then x else 0
```

###### `let in`:

```
let x = 0 in x
let rec x = (x, 0)
```

###### Top-level definitions

```
def x = fun x -> x
```

Top-level definitions are implicitly recursive.

###### Built-in operators and functions:

* Arithmetic operators: `+`, `-`, `*`, `/`, `>, `>=`, `<`, `<=`
* Polymorphic compareason operator: `=`
* Cons operator: `::`
* List accessors: `hd`, `tl`
* Tuple accessors (for 2-uples): `hd`, `tl`
* Prints an integer followed by a newline: `print_int`
