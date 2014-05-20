semiring
========

A functional semiring library.

File `semiring.ml`

```ocaml

(** The general interface of a semiring *)
Semiring.S

(** The functor of a matrix semiring *)
Semiring.Make_Matrix_Semiring(A_SEMIRING)

```

MinplusSemiring: `corebuild minplus.byte`

```ocaml

(** MinPlusSemiring *)
MPS
(** Matrix MinPlusSemiring *)
MMPS

```

BooleanSemiring: `corebuild -pkg str minplus.byte`

```ocaml
(** BooleanSemiring *)
BS
(** Matrix BooleanSemiring *)
MBS

```

Martelli: `corebuild -pkg str minplus.byte`

```ocaml

(** MartelliSemiring *)
MS
(** Matrix MartelliSemiring *)
MMS

```
