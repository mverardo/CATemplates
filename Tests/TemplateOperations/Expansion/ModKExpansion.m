(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {}, ModKExpansion, 2]},
      ModKExpansion[T] === {{}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {2 - x0, x0}, ModKExpansion, 2]},
      ModKExpansion[T] === {{0, 0}, {1, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {3 - x0, x0}, ModKExpansion, 2]},
      ModKExpansion[T] === {{1, 0}, {0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {3 - x0, x0}, ModKExpansion, 2]},
      ModKExpansion[T] === {{0, 0}, {2, 1}, {1, 2}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x2, x2, 1 + x0, x0}, ModKExpansion, 2]},
      ModKExpansion[T] === {
        {1, 0, 1, 0}, {1, 0, 2, 1}, {1, 0, 0, 2}, {2, 1, 1, 0},
        {2, 1, 2, 1}, {2, 1, 0, 2}, {0, 2, 1, 0}, {0, 2, 2, 1}, {0, 2, 0, 2}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1}, ModKExpansion, 2]},
      ModKExpansion[T] === {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, ModKExpansion, 2]},
      Sort @ ModKExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, ModKExpansion, 2]},
      ModKExpansion[T, 0] === {0, 1, 0, 1, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, ModKExpansion, 2]},
      ModKExpansion[T, 1] === {0, 1, 0, 1, 0, 1, 0, 1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}, ModKExpansion, 2]},
      Sort[ModKExpansion[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1 + x0}, ModKExpansion, 2]},
      Sort @ ModKExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, ModKExpansion, 3]},
      Sort @ ModKExpansion[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, ModKExpansion, 2]},
      Sort @ ModKExpansion[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, {0, 0, 1, 0, 0, 1, 0, 0}}]]}];

PrintTestResults[report];
