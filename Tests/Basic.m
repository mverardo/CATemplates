(* ::Package:: *)

<<CATemplates`

Print["RawCore"];

rawTemplateReport = TestReport[{
  VerificationTest[RawCore[BaseTemplateCore[]] === BaseTemplateCore[]],
  VerificationTest[RawCore[{x8, 0, x6, 0, x4, 0, x2, 0}] === {x8, 0, x6, 0, x4, 0, x2, 0}],
  VerificationTest[RawCore[{x2 \[Element] {0, 2}, x0, x0}] === {x2, x0, x0}],
  VerificationTest[RawCore[{x2, x1 \[Element] {0, 2}, x0}] === {x2, x1, x0}]}];

PrintTestResults[rawTemplateReport];