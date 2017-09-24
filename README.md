# OCaml Ray Tracer
By Aaron Fritz

OCaml is the parent language of F#, and between the two of them, I would say I prefer the latter to be honest. OCaml's type system is very strong and expressive, but these days its most common compiler implementation is lagging behind others due to its lack of certain features like multi-threaded parallelism. To me, F# just feels easier to read and write (_and_ it has parallelism from .NET as well). However, OCaml's native code generator (via `ocamlopt`) is one feature it has over the leading F# implementation, so that's one reason why I have some interest writing OCaml code. In any case, I like how all values are immutable by default, and how the code I write generally turns out to be fairly terse.

![Image](image.png)

Written with Visual Studio 2017 and compiled with `ocamlopt` in Cygwin. The program outputs a 720p .PPM image that can be converted to another format like PNG.
