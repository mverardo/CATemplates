(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1}, RawExpansion]},
  Print[RawExpansion[T] === {{0,1,0,1,0,1,0,1}}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, RawExpansion]},
  Print[Sort @ RawExpansion[T] === Sort @ {{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, RawExpansion]},
  Print[RawExpansion[T, 0] === {0,1,0,1,0,1,0,0}]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, RawExpansion]},
  Print[RawExpansion[T, 1] === {0,1,0,1,0,1,0,1}]
];

With[
  {T = BuildTemplate[2, 1.0, {x7,1,0,1,0,1,0,x0}, RawExpansion]},
  Print[Sort[RawExpansion[T]] === Sort[{{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}, {1,1,0,1,0,1,0,1}, {1,1,0,1,0,1,0,0}}]]
];

With[
  {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1+x0}, RawExpansion]},
  Print[Sort @ RawExpansion[T] === Sort @ {{0,1,0,1,0,1,0,2}, {0,1,0,1,0,1,0,1}}]
];