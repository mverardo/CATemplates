(* ::Package:: *)

<< CATemplates`;

Print["BuildTemplate"];

buildTemplateReport = TestReport[{
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "rawList" -> {x7, x6, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> Identity|>},
      BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}, Identity] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Identity|>},
      BuildTemplate[2, 1.0, {x7, 1, x5, x4, 1, x2, x1, 0}, Identity] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2]|>},
      BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2]] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2], "N" -> 2|>},
      BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 2] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2], "N" -> 3|>},
      BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 3] === expectedTemplate]]}];

PrintTestResults[buildTemplateReport];

baseTemplateReport = TestReport[{
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]},
      BaseTemplate[2, 1.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.5, {x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]},
      BaseTemplate[2, 1.5] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 2.0, {x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]},
      BaseTemplate[2, 2.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]},
      BaseTemplate[2, 1.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 1.5, {x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]},
      BaseTemplate[2, 1.5] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = BuildTemplate[2, 2.0, {x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]},
      BaseTemplate[2, 2.0] === expectedTemplate]]}];

PrintTestResults[baseTemplateReport];

accessorFnsReport = TestReport[{
  VerificationTest[k[BaseTemplate[2, 1.0]] === 2],
  VerificationTest[k[BaseTemplate[3, 1.0]] === 3],
  VerificationTest[r[BaseTemplate[2, 1.0]] === 1.0],
  VerificationTest[r[BaseTemplate[2, 2.0]] === 2.0],
  VerificationTest[expansionFunction[BaseTemplate[2, 2.0]] === RawExpansion],
  VerificationTest[kAryRuleTemplate[BaseTemplate[2, 1.0]] === {x7,x6,x5,x4,x3,x2,x1,x0}]
}];

PrintTestResults[accessorFnsReport];
