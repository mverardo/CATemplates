(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1}, ModNExpansion, 2]},
  Print[ModNExpansion[T] === {{0,1,0,1,0,1,0,1}}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, ModNExpansion, 2]},
  Print[Sort @ ModNExpansion[T] === Sort @ {{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, ModNExpansion, 2]},
  Print[ModNExpansion[T, 0] === {0,1,0,1,0,1,0,0}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, ModNExpansion, 2]},
  Print[ModNExpansion[T, 1] === {0,1,0,1,0,1,0,1}]
];

With[
  {T = BuildTemplate[2, 1.0, {x7,1,0,1,0,1,0,x0}, ModNExpansion, 2]},
  Print[Sort[ModNExpansion[T]] === Sort[{{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}, {1,1,0,1,0,1,0,1}, {1,1,0,1,0,1,0,0}}]]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1+x0}, ModNExpansion, 2]},
  Print[Sort @ ModNExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]
];

With[
  {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, ModNExpansion, 3]},
  Print[Sort @ ModNExpansion[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, ModNExpansion, 2]},
  Print[Sort @ ModNExpansion[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, {0, 0, 1, 0, 0, 1, 0, 0}}]
];

