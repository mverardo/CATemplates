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
      Sort[ExpandTemplate[T]] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, BaseTemplateCore[]]},
      ExpandTemplate[T] === Tuples[{0, 1}, 8]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, BaseTemplateCore[]]},
      ExpandTemplate[T, 0] === {0, 0, 0, 0, 0, 0, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, BaseTemplateCore[]]},
      ExpandTemplate[T, 4] === {0, 0, 0, 0, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, x1, 0, 1}]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 0, 1}, {0, 1, 1, 0, 1}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {0, 2, x1, 0, 1}]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 2, 0, 0, 1}, {0, 2, 1, 0, 1}, {0, 2, 2, 0, 1}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {0, 1, 0, 2, x1}]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 2, 0}, {0, 1, 0, 2, 1}, {0, 1, 0, 2, 2}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1}]},
      ExpandTemplate[T] === {{0, 1, 0, 1, 0, 1, 0, 1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}]},
      ExpandTemplate[T, 0] === {0, 1, 0, 1, 0, 1, 0, 0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, x0}]},
      ExpandTemplate[T, 1] === {0, 1, 0, 1, 0, 1, 0, 1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7, 1, 0, 1, 0, 1, 0, x0}]},
      Sort[ExpandTemplate[T]] === Sort[{{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}, {1, 1, 0, 1, 0, 1, 0, 1}, {1, 1, 0, 1, 0, 1, 0, 0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0, 1, 0, 1, 0, 1, 0, 1 + x0}]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 2}, {0, 1, 0, 1, 0, 1, 0, 1}}]]}];

Print["ExpandTemplate"]
PrintReport[report];

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
PrintReport[FilterOutOfRangeReport];

filterNotAllowedReport = TestReport[{
  VerificationTest[
    ExpandTemplate[BuildTemplate[2, 1.0, {0, 1, x1 \[Element] {0, 2}, 1, 0}, FilterNotAllowed]] === {{0, 1, 0, 1, 0}}],
  VerificationTest[
    ExpandTemplate[BuildTemplate[2, 1.0, {0, 1, x1 \[Element] {0, 1}, 1, 0}, FilterNotAllowed]] === {{0, 1, 0, 1, 0}, {0, 1, 1, 1, 0}}],
  VerificationTest[
    ExpandTemplate[BuildTemplate[2, 1.0, {0, x1, x2 \[Element] {0, 1}, 2 - x2}, FilterNotAllowed]] === {{0, 0, 0, 2}, {0, 1, 0, 2}, {0, 0, 1, 1}, {0, 1, 1, 1}}],
  VerificationTest[
    ExpandTemplate[BuildTemplate[3, 1.0, {0, 2, x1 \[Element] {0, 2}, 0, 1}, FilterNotAllowed]] === {{0, 2, 0, 0, 1}, {0, 2, 2, 0, 1}}],
  VerificationTest[
    ExpandTemplate[BuildTemplate[3, 1.0, {0, 2, x1 \[Element] {1, 2}, 0, 1}, FilterNotAllowed]] === {{0, 2, 1, 0, 1}, {0, 2, 2, 0, 1}}],
  VerificationTest[
    ExpandTemplate[BuildTemplate[3, 1.0, {0, 2, x1 \[Element] {0, 1}, 0, 1}, FilterNotAllowed]] === {{0, 2, 0, 0, 1}, {0, 2, 1, 0, 1}}]}]

Print["ExpandTemplate + FilterNotAllowed"];
PrintReport[filterNotAllowedReport];

modKReport = TestReport[{
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
PrintReport[modKReport];

TemplateModReport = TestReport[{
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1}, TemplateMod, 2]},
      ExpandTemplate[T] === {{0,1,0,1,0,1,0,1}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, TemplateMod, 2]},
      Sort @ ExpandTemplate[T] === Sort @ {{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, TemplateMod, 2]},
      ExpandTemplate[T, 0] === {0,1,0,1,0,1,0,0}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,x0}, TemplateMod, 2]},
      ExpandTemplate[T, 1] === {0,1,0,1,0,1,0,1}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {x7,1,0,1,0,1,0,x0}, TemplateMod, 2]},
      Sort[ExpandTemplate[T]] === Sort[{{0,1,0,1,0,1,0,1}, {0,1,0,1,0,1,0,0}, {1,1,0,1,0,1,0,1}, {1,1,0,1,0,1,0,0}}]]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {0,1,0,1,0,1,0,1+x0}, TemplateMod, 2]},
      Sort @ ExpandTemplate[T] === Sort @ {{0, 1, 0, 1, 0, 1, 0, 1}, {0, 1, 0, 1, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[3, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 0}, TemplateMod, 3]},
      Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 0}, {2, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 0}}]],
  VerificationTest[
    With[
      {T = BuildTemplate[2, 1.0, {1 + x7, 0, 1, 0, 0, 1, 0, 1 - x0}, TemplateMod, 2]},
      Sort @ ExpandTemplate[T] === Sort @ {{1, 0, 1, 0, 0, 1, 0, 1}, {1, 0, 1, 0, 0, 1, 0, 0}, {0, 0, 1, 0, 0, 1, 0, 1}, {0, 0, 1, 0, 0, 1, 0, 0}}]]}];

Print["ExpandTemplate + TemplateMod"];
PrintReport[TemplateModReport];
