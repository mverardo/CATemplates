(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1}, FilteredExpansion]},
  Print[FilteredExpansion[T] === {{0,1,0,1,0,1,0,1}}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, FilteredExpansion]},
  Print[Sort @ FilteredExpansion[T] === Sort @ {{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, FilteredExpansion]},
  Print[FilteredExpansion[T, 0] === {0,1,0,1,0,1,0,0}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, FilteredExpansion]},
  Print[FilteredExpansion[T, 1] === {0,1,0,1,0,1,0,1}]
];

With[
  {T = BuildTemplate[2, 1.0, {x7,1,0,1,0,1,0,x0}, FilteredExpansion]},
  Print[Sort[FilteredExpansion[T]] === Sort[{{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}, {1,1,0,1,0,1,0,1}, {1,1,0,1,0,1,0,0}}]]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1+x0}, FilteredExpansion]},
  Print[Sort @ FilteredExpansion[T] === Sort @ {{0,1,0,1,0,1,0,1}}]
];

With[
  {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, FilteredExpansion]},
  Print[Sort @ FilteredExpansion[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}}]
];