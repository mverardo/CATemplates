(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 1}, {2, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, FilterOutOfRange]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, ModK]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, ModK, 3]},
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]]}];

Print["ExpandTemplate"]
PrintTestResults[report];

FilterOutOfRangeReport = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1}, FilterOutOfRange]},
      ExpandTemplate[T] === {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, FilterOutOfRange]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, FilterOutOfRange]},
      ExpandTemplate[T, 0] === {0, 1, 0, 1, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, FilterOutOfRange]},
      ExpandTemplate[T, 1] === {0, 1, 0, 1, 0, 1, 0, 1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}, FilterOutOfRange]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1 + x0}, FilterOutOfRange]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, FilterOutOfRange]},
      Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}}]]}];

Print["ExpandTemplate + FilterOutOfRange"];
PrintTestResults[FilterOutOfRangeReport];

modKReport = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {}, ModK, 2]},
      ExpandTemplate[T] === {{}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {2 - x0, x0}, ModK, 2]},
      ExpandTemplate[T] === {{0, 0}, {1, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {3 - x0, x0}, ModK, 2]},
      ExpandTemplate[T] === {{1, 0}, {0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {3 - x0, x0}, ModK, 2]},
      ExpandTemplate[T] === {{0, 0}, {2, 1}, {1, 2}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x2, x2, 1 + x0, x0}, ModK, 2]},
      ExpandTemplate[T] === {
        {1, 0, 1, 0}, {1, 0, 2, 1}, {1, 0, 0, 2}, {2, 1, 1, 0},
        {2, 1, 2, 1}, {2, 1, 0, 2}, {0, 2, 1, 0}, {0, 2, 2, 1}, {0, 2, 0, 2}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1}, ModK, 2]},
      ExpandTemplate[T] === {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, ModK, 2]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, ModK, 2]},
      ExpandTemplate[T, 0] === {0, 1, 0, 1, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, ModK, 2]},
      ExpandTemplate[T, 1] === {0, 1, 0, 1, 0, 1, 0, 1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}, ModK, 2]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1 + x0}, ModK, 2]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, ModK, 3]},
      Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, ModK, 2]},
      Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, {0, 0, 1, 0, 0, 1, 0, 0}}]]}];

Print["ExpandTemplate + ModK"];
PrintTestResults[modKReport];
