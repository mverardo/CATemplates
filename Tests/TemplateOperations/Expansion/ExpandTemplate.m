(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<<CATemplates`;

With[
  {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, RawExpansion]},
  Print[Sort @ ExpandTemplate[T] === Sort @ {{1,0,1,0,0,1,0,1},{1,0,1,0,0,1,0,0},{2,0,1,0,0,1,0,1},{2,0,1,0,0,1,0,0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, FilteredExpansion]},
  Print[Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, ModKExpansion]},
  Print[Sort @ ExpandTemplate[T] === Sort @ {{1,0,1,0,0,1,0,1},{1,0,1,0,0,1,0,0},{0,0,1,0,0,1,0,1},{0,0,1,0,0,1,0,0}}]
];

With[
  {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, ModKExpansion, 3]},
  Print[Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {x7,1,0,1,0,1,0,x0}, RawExpansion]},
  Print[Sort[ExpandTemplate[T]] === Sort[{{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}, {1,1,0,1,0,1,0,1}, {1,1,0,1,0,1,0,0}}]]
];