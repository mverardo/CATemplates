(* ::Package:: *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[{
      t1 = BaseTemplate[2, 1.0],
      t2 = BuildTemplate[2, 1.0, ConstantArray[0, 8]]},
      TemplateIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = BaseTemplate[2, 1.0],
      t2 = BuildTemplate[2, 1.0, ConstantArray[1, 8]]},
      TemplateIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = BaseTemplate[2, 1.0]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BaseTemplate[2, 2.0]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BaseTemplate[2, 3.0]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      t2 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      result = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BaseTemplate[2, 1.0],
      t2 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      result = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}]},
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {x7, 0, x5, 0, x3, 0, x1, 0}],
      t2 = BaseTemplate[2, 1.0],
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
      TemplateIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[2, 1.0, {1, x, 1, 0}],
      t2 = BuildTemplate[2, 1.0, {1, 1, 1-x, x}],
      result = BuildTemplate[2, 1.0, {1, 1, 1, 0}]},
      TemplateIntersection[t1, t2] === result]]}];

PrintReport[report];

Print["mod"];

modReport = TestReport[{
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, ModK]},
      TemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, ModK],
      t2 = BuildTemplate[3, 1.0, {1, x0}, ModK]},
      TemplateIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x2, 1, x0}, ModK],
      t2 = BuildTemplate[3, 1.0, {x2, x1, x0}, ModK]},
      TemplateIntersection[t1, t2] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, ModK],
      t2 = BuildTemplate[3, 1.0, {1, x0}, ModK],
      t3 = BuildTemplate[3, 1.0, {0, x0}, ModK]},
      With[{i1 = TemplateIntersection[t1, t2],
        i2 = TemplateIntersection[t1, t3],
        result = BuildTemplate[3, 1.0, {}, ModK]},
        TemplateIntersection[i1, i2] === result]]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, ModK],
      t2 = BuildTemplate[3, 1.0, {1, x0}, ModK],
      t3 = BuildTemplate[3, 1.0, {x1, 0}, ModK]},
      With[{
        i1 = TemplateIntersection[t1, t2],
        i2 = TemplateIntersection[t1, t3],
        result = BuildTemplate[3, 1.0, {1, 0}, ModK]},
        TemplateIntersection[i1, i2] === result]]],
  VerificationTest[(*Should work even if ModK is in a list of postExpansionFns*)
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, {FilterNotAllowed, ModK}],
      t2 = BuildTemplate[3, 1.0, {4, x0}, {FilterNotAllowed, ModK}],
      t3 = BuildTemplate[3, 1.0, {x1, 0}, {FilterNotAllowed, ModK}]},
      With[{
        i1 = TemplateIntersection[t1, t2],
        i2 = TemplateIntersection[t1, t3],
        result = BuildTemplate[3, 1.0, {1, 0}, {FilterNotAllowed, ModK}]},
        TemplateIntersection[i1, i2] === result]]]}];

PrintReport[modReport];

Print["CoreVarsFromConstants"];

CoreVarsFromConstantsReport = TestReport[{
  VerificationTest[
    CoreVarsFromConstants[{x0 -> C[1]}] === {x0 -> x0}],
  VerificationTest[
    CoreVarsFromConstants[{x1 -> C[2], x0 -> C[1]}] === {x1 -> x1, x0 -> x0}],
  VerificationTest[
    CoreVarsFromConstants[{x2 -> 1 + C[1], x1 -> C[2], x0 -> C[1]}] === {x2 -> 1 + x0, x1 -> x1, x0 -> x0}],
  VerificationTest[
    CoreVarsFromConstants[{x3 -> 1 + C[1] + C[2], x2 -> 1 + C[1], x1 -> C[2], x0 -> C[1]}] == {x3 -> 1 + x0 + x1, x2 -> 1 + x0,   x1 -> x1, x0 -> x0}]}]

PrintReport[CoreVarsFromConstantsReport];

Print["EquationsFromValueRestrictions"];

valueRestrictionsReport = TestReport[{
  VerificationTest[EquationsFromValueRestrictions[x1 \[Element] {0, 1}] === (x1 == 0 || x1 == 1)],
  VerificationTest[EquationsFromValueRestrictions[x2 \[Element] {0, 1}] === (x2 == 0 || x2 == 1)],
  VerificationTest[EquationsFromValueRestrictions[x2 \[Element] {0, 1, 2}] === (x2 == 0 || x2 == 1 || x2 == 2)]
}];

PrintReport[valueRestrictionsReport];
