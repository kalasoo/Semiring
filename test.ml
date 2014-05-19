open Util

module MMPS = Semiring.Make_Matrix_Semiring(MPS)

let () =
  let example = Array.make_matrix 3 3 MPS.zero in
  example.(0).(1) <- MPS.Val 2;
  example.(0).(2) <- MPS.Val 4;
  example.(1).(2) <- MPS.Val 1;

  MMPS.solve example
