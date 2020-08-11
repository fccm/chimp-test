# Chimpanzee Memory Test

https://www.youtube.com/watch?v=zsXP8qeFF6A

Ayumu the chimpanzee has made headlines around the world for his ability to beat
humans on memory tests, in both speed and accuracy. Does Ayumu's ability force
us to reconsider our assumptions about human superiority to other primates?


# Execution

You need ocamlsdl2 to run and/or compile:  
https://github.com/fccm/OCamlSDL2

This version of the program is known to work with:
- SDL2 version 2.0.10
- OCaml version 4.09.0
- OCamlSDL2 version 0.03

After installing ocamlsdl2 you can run the program with:
```
ocaml -I $(ocamlfind query sdl2) sdl2.cma chimp_test.ml
```

If you just compiled ocamlsdl2 without installing it:
```
ocaml -I ../OCamlSDL2/src sdl2.cma chimp_test.ml
```


# License

This program is released under a restrictionless Zlib license,  
see the file LICENSE.txt for details.


# Comments

Write to me if you have comments about this program:  
monnier.florent (at) gmail.com
