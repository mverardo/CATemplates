(* ::Package:: *)

<< CATemplates`;

Print["BuildTemplate"];

buildTemplateReport = TestReport[{
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "core" -> {x7, x6, x5, x4, x3, x2, x1, x0}, "postExpansionFn" -> IdentityFn|>},
      BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "core" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "postExpansionFn" -> IdentityFn|>},
      BuildTemplate[2, 1.0, {x7, 1, x5, x4, 1, x2, x1, 0}] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "core" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "postExpansionFn" -> Function[{x,y}, 2] |>},
      BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2]] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "core" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "postExpansionFn" -> Function[{x,y}, 2], "N" -> 2|>},
      BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 2] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "core" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "postExpansionFn" -> Function[{x,y}, 2], "N" -> 3|>},
      BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 3] === expectedTemplate]]}];

PrintReport[buildTemplateReport];

Print["BaseTemplate"];

baseTemplateReport = TestReport[{
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}]},
      BaseTemplate[2, 1.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.5, {x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}]},
      BaseTemplate[2, 1.5] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 2.0, {x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}]},
      BaseTemplate[2, 2.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}]},
      BaseTemplate[2, 1.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.5, {x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}]},
      BaseTemplate[2, 1.5] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 2.0, {x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}]},
      BaseTemplate[2, 2.0] === expectedTemplate]]}];

PrintReport[baseTemplateReport];

Print["ValidTemplateCoreQ"];

ValidTemplateCoreQReport = TestReport[{
  VerificationTest[ValidTemplateCoreQ[{1,2,3,4,5}] === True],
  VerificationTest[ValidTemplateCoreQ[BaseTemplateCore[]] === True],
  VerificationTest[ValidTemplateCoreQ[{1x2, 2x1, 3x0}] === True],
  (* We can have fractions as long as they are multiplying a variable *)
  VerificationTest[ValidTemplateCoreQ[{1,2,3,4,5/2x1}] === True],
  VerificationTest[ValidTemplateCoreQ[{1,2,3-x2,4+x1,5/2x1}] === True],
  VerificationTest[ValidTemplateCoreQ[{1/2,2,3,4,5}] === False],
  VerificationTest[ValidTemplateCoreQ[{1,2/4,3-x2,4+x1,5/2x1}] === False],
  VerificationTest[ValidTemplateCoreQ[{1,1.0,3-x2,4,5}] === False],
  VerificationTest[ValidTemplateCoreQ[{1,2,"bla",4,5}] === False]}];

PrintReport[ValidTemplateCoreQReport];

Print["TemplateCoreVars"];

TemplateCoreVarsReport = TestReport[{
  VerificationTest[TemplateCoreVars[{1,2,3,4}] === {}],
  VerificationTest[TemplateCoreVars[{1,2,3,x0}] === {x0}],
  VerificationTest[TemplateCoreVars[{x3,2,3,x0}] === {x0, x3}],
  VerificationTest[TemplateCoreVars[{y3,2,3,y0}] === {y0, y3}],
  VerificationTest[TemplateCoreVars[{x3 \[Element] {1,0},2,3,x0}] === {x0, x3}],
  VerificationTest[TemplateCoreVars[BaseTemplateCore[]] === {x0, x1, x2, x3, x4, x5, x6, x7}],
  VerificationTest[TemplateCoreVars[BaseTemplate[2, 1.0]] === {x0, x1, x2, x3, x4, x5, x6, x7}]
}];

PrintReport[TemplateCoreVarsReport];

Print["RawCore"];

RawCoreReport = TestReport[{
  VerificationTest[RawCore[BaseTemplateCore[]] === BaseTemplateCore[]],
  VerificationTest[RawCore[{x8, 0, x6, 0, x4, 0, x2, 0}] === {x8, 0, x6, 0, x4, 0, x2, 0}],
  VerificationTest[RawCore[{x2 \[Element] {0, 2}, x0, x0}] === {x2, x0, x0}],
  VerificationTest[RawCore[{x2, x1 \[Element] {0, 2}, x0}] === {x2, x1, x0}]}];

PrintReport[RawCoreReport];

Print["accessorFns"];

accessorFnsReport = TestReport[{
  VerificationTest[k[BaseTemplate[2, 1.0]] === 2],
  VerificationTest[k[BaseTemplate[3, 1.0]] === 3],
  VerificationTest[r[BaseTemplate[2, 1.0]] === 1.0],
  VerificationTest[r[BaseTemplate[2, 2.0]] === 2.0],
  VerificationTest[postExpansionFn[BuildTemplate[2, 2.0, {}, FilterOutOfRange]] === FilterOutOfRange],
  VerificationTest[templateCore[BaseTemplate[2, 1.0]] === {x7,x6,x5,x4,x3,x2,x1,x0}],
  VerificationTest[valueRestrictions[BuildTemplate[2, 1.0, {x8, 0, x6, 0, x4, 0, x2, 0}]] === {}],
  VerificationTest[valueRestrictions[BuildTemplate[2, 1.0, {x2 \[Element] {0, 2}, x0, x0}]]  === {x2 \[Element] {0, 2}}],
  VerificationTest[valueRestrictions[BuildTemplate[2, 1.0, {x2, x1 \[Element] {0, 2}, x0}]] === {x1 \[Element] {0, 2}}],
  VerificationTest[valueRestrictions[BuildTemplate[2, 1.0, {x2 \[Element] {0, 1}, x1 \[Element] {0, 2}, x0}]] === {x2 \[Element] {0, 1}, x1 \[Element] {0, 2}}]
}];

PrintReport[accessorFnsReport];
