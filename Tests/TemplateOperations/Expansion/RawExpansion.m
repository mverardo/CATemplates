(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {}, RawExpansion]},
      RawExpansion[T] === {{}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, OldBaseTemplate[], RawExpansion]},
      RawExpansion[T] === Tuples[{0, 1}, 8]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, OldBaseTemplate[], RawExpansion]},
      RawExpansion[T, 0] === {0, 0, 0, 0, 0, 0, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, OldBaseTemplate[], RawExpansion]},
      RawExpansion[T, 4] === {0, 0, 0, 0, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, x1, 0, 1}, RawExpansion]},
      Sort[RawExpansion[T]] === Sort[{{0, 1, 0, 0, 1}, {0, 1, 1, 0, 1}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {0, 2, x1, 0, 1}, RawExpansion]},
      Sort[RawExpansion[T]] === Sort[{{0, 2, 0, 0, 1}, {0, 2, 1, 0, 1}, {0, 2, 2, 0, 1}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {0, 1, 0, 2, x1}, RawExpansion]},
      Sort[RawExpansion[T]] === Sort[{{0, 1, 0, 2, 0}, {0, 1, 0, 2, 1}, {0, 1, 0, 2, 2}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1}, RawExpansion]},
      RawExpansion[T] === {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, RawExpansion]},
      Sort @ RawExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, RawExpansion]},
      RawExpansion[T, 0] === {0, 1, 0, 1, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}, RawExpansion]},
      RawExpansion[T, 1] === {0, 1, 0, 1, 0, 1, 0, 1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}, RawExpansion]},
      Sort[RawExpansion[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1 + x0}, RawExpansion]},
      Sort @ RawExpansion[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 2}, {0, 1, 0, 1, 0, 1, 0, 1}}]]}];

PrintTestResults[report];
