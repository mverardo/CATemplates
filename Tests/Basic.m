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

Print["ExceptionTemplates"];

exceptionTemplatesReport = TestReport[{
  VerificationTest[ExceptionTemplates[{x7, x6, x5, x4, x3, x2, x1, x0}] === {}],
  VerificationTest[ExceptionTemplates[{x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x2 - x1}, 3] === {{x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 1, x0}, {x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 2, x0}, {x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 1, 2, x0}}],
  VerificationTest[ExceptionTemplates[{x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x2 - x1}, 2, 2] === {{x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 1, x0}}]
}];

PrintTestResults[exceptionTemplatesReport];