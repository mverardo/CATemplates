(* ::Package:: *)

<<CATemplates`

Print["RawTemplate"];

rawTemplateReport = TestReport[{
  VerificationTest[RawTemplate[OldBaseTemplate[]] === OldBaseTemplate[]],
  VerificationTest[RawTemplate[{x8, 0, x6, 0, x4, 0, x2, 0}] === {x8, 0, x6, 0, x4, 0, x2, 0}],
  VerificationTest[RawTemplate[{x2 \[Element] {0, 2}, x0, x0}] === {x2, x0, x0}],
  VerificationTest[RawTemplate[{x2, x1 \[Element] {0, 2}, x0}] === {x2, x1, x0}]}];

PrintTestResults[rawTemplateReport];

Print["BooleanFromRule"];

report = TestReport[{
  VerificationTest[BooleanFromRule[x1->0] == Not[x1]],
  VerificationTest[BooleanFromRule[x1->1] == x1],
  VerificationTest[BooleanFromRule[y->1] == y]
}];

PrintTestResults[report];

Print["ConjunctionFromRuleSet"];

report = TestReport[{
  VerificationTest[ConjunctionFromRuleSet[{}] == True],
  VerificationTest[ConjunctionFromRuleSet[{x1->0}] == Not[x1]],
  VerificationTest[ConjunctionFromRuleSet[{x1->0, x2 -> 1}] == (Not[x1] && x2)]
}];

PrintTestResults[report];

Print["DNFFromRuleSet"];

report = TestReport[{
  VerificationTest[DNFFromRuleSet[{{}}] == True],
  VerificationTest[DNFFromRuleSet[{{x1->0}}] == Not[x1]],
  VerificationTest[DNFFromRuleSet[{{x1->0, x2 -> 1}}] == (Not[x1] && x2)],
  VerificationTest[DNFFromRuleSet[{{x1->0, x2 -> 1}, {x1->1}, {x0->0}}] == ((Not[x1] && x2) || x1 || Not[x0])]
}];

PrintTestResults[report];

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

