(* ::Package:: *)

<<CATemplates`

Print["RawTemplate"];

rawTemplateReport = TestReport[{
  VerificationTest[RawTemplate[BaseTemplateCore[]] === BaseTemplateCore[]],
  VerificationTest[RawTemplate[{x8, 0, x6, 0, x4, 0, x2, 0}] === {x8, 0, x6, 0, x4, 0, x2, 0}],
  VerificationTest[RawTemplate[{x2 \[Element] {0, 2}, x0, x0}] === {x2, x0, x0}],
  VerificationTest[RawTemplate[{x2, x1 \[Element] {0, 2}, x0}] === {x2, x1, x0}]}];

PrintTestResults[rawTemplateReport];

Print["ImprisonmentExpressions"];

imprisonmentExpressionsReport = TestReport[{
  VerificationTest[ImprisonmentExpressions[{x8, 0, x6, 0, x4, 0, x2, 0}] == {}],
  VerificationTest[ImprisonmentExpressions[{x2 \[Element] {0, 2}, x0, x0}]  === {x2 \[Element] {0, 2}}],
  VerificationTest[ImprisonmentExpressions[{x2, x1 \[Element] {0, 2}, x0}] === {x1 \[Element] {0, 2}}],
  VerificationTest[ImprisonmentExpressions[{x2 \[Element] {0, 1}, x1 \[Element] {0, 2}, x0}] === {x2 \[Element] {0, 1}, x1 \[Element] {0, 2}}]
}];

PrintTestResults[imprisonmentExpressionsReport];

Print["Value Restrictions"];

valueRestrictionsReport = TestReport[{
  VerificationTest[ValueRestrictions[x1 \[Element] {0, 1}] === (x1 == 0 || x1 == 1)],
  VerificationTest[ValueRestrictions[x2 \[Element] {0, 1}] === (x2 == 0 || x2 == 1)],
  VerificationTest[ValueRestrictions[x2 \[Element] {0, 1, 2}] === (x2 == 0 || x2 == 1 || x2 == 2)]
}];

PrintTestResults[valueRestrictionsReport];