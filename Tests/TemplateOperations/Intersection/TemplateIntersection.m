(* ::Package:: *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, OldBaseTemplate[]],
      t2 = BuildTemplate[2, 1.0, ConstantArray[0, 8]]},
      TemplateIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, OldBaseTemplate[]],
      t2 = BuildTemplate[2, 1.0, ConstantArray[1, 8]]},
      TemplateIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, OldBaseTemplate[]]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, OldBaseTemplate[2, 2]]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, OldBaseTemplate[2, 3]]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      t2 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      result = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, OldBaseTemplate[]],
      t2 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      result = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      t2 = BuildTemplate[2, 1.0, OldBaseTemplate[]],
      result = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x1, x0}, RestrictedExpansion]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x1, x0}],
      t2 = BuildTemplate[2, 1.0, {x2, x0, x0}],
      result = BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x0, x0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x1, x0}],
      t2 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 1}, x0, x0}],
      result = BuildTemplate[2, 1.0, {0, x0, x0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2, x1 \[Element] {0, 2}, x0}],
      t2 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 1}, x0, x0}],
      result = BuildTemplate[2, 1.0, {x2 \[Element] {0, 1}, x0 \[Element] {0, 2}, x0 \[Element] {0, 2}}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x1, x0}],
      t2 = BuildTemplate[2, 1.0, {0, x0, x0}],
      result = BuildTemplate[2, 1.0, {0, x0, x0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {2}, x1, x0}],
      t2 = BuildTemplate[2, 1.0, {2, x0, x0}],
      result = BuildTemplate[2, 1.0, {2, x0, x0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {2}, x1, x0}],
      t2 = BuildTemplate[2, 1.0, {0, x0, x0}],
      result = BuildTemplate[2, 1.0, {}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {2, 3}, x1, x0}],
      t2 = BuildTemplate[2, 1.0, {x2 \[Element] {1, 2, 3}, x0, x0}],
      result = BuildTemplate[2, 1.0, {x2 \[Element] {2, 3}, x0, x0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x1 \[Element] {0, 1}, x0}],
      t2 = BuildTemplate[2, 1.0, {x2  \[Element] {0, 1}, x0 \[Element] {1, 2}, x0}],
      result = BuildTemplate[2, 1.0, {0, 1, 1}]},
      TemplateIntersection[t1, t2] === result]]}];

PrintTestResults[report];
