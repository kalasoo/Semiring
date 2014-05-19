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
(** Matrix MinPlusSemiring *)
Util.MMPS

(** BooleanSemiring *)
Util.BS
(** Matrix BooleanSemiring *)
Util.MBS

(** MartelliSemiring *)
Util.MS
(** Matrix MartelliSemiring *)
Util.MMS

```

Compilation
-----------

```shell

corebuild util.byte

```
