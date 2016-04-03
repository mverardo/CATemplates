(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, RawExpansion]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 1}, {2, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, FilteredExpansion]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, ModKExpansion]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, ModKExpansion, 3]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}, RawExpansion]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]]}];

PrintTestResults[report];