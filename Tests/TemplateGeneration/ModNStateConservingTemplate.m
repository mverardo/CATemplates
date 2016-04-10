(* ::Package:: *)

<< CATemplates`;
<< CATemplates`TemplateOperations`ExpandTemplate`;

report = TestReport[{
  VerificationTest[Sort[FromDigits[#, 2] & /@ ExpandTemplateModK[ModNStateConservingTemplate[2], 2]] == {132, 150, 170, 184, 204, 222, 226, 240}],
  VerificationTest[PreservesIndexVariableDualityQ[ModNStateConservingTemplate[2]]],
  (*VerificationTest[Sort[FromDigits[#,2] & /@ ExpandTemplateModK[ModNStateConservingTemplate[3], 2]] == {170, 184, 204, 226, 240}],*)
  VerificationTest[PreservesIndexVariableDualityQ[ModNStateConservingTemplate[3]]]
}];

PrintTestResults[report];