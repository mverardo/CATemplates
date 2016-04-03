(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1}, FilteredExpansion]},
      FilteredExpansion[T] === {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, FilteredExpansion]},
      Sort @ FilteredExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, FilteredExpansion]},
      FilteredExpansion[T, 0] === {0, 1, 0, 1, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, FilteredExpansion]},
      FilteredExpansion[T, 1] === {0, 1, 0, 1, 0, 1, 0, 1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}, FilteredExpansion]},
      Sort[FilteredExpansion[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1 + x0}, FilteredExpansion]},
      Sort @ FilteredExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, FilteredExpansion]},
      Sort @ FilteredExpansion[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}}]]}];

PrintTestResults[report];
