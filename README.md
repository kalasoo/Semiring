semiring
========

A functional semiring library.

File `semiring.ml`

```ocaml

(** The general interface of a semiring *)
Semiring.S

(** The general interface of a semiring *)
Semiring.Make_Matrix_Semiring(A_SEMIRING)

```

File `util.ml`

```ocaml

(** MinPlusSemiring *)
Util.MPS

(** BooleanSemiring *)
Util.BS

(** MartelliSemiring *)
Util.MS

```

Compilation
-----------

```shell

corebuild util.byte

```
