(* ::Package:: *)

<< CATemplates`;


Print["PossibleInvalidSubsets"]
possibleInvalidSubsets = TestReport[{
  VerificationTest[PossibleInvalidSubsets[{x7, x6, x5, x4, x3, x2, x1, x0}] === {}],
  VerificationTest[PossibleInvalidSubsets[{x7, x6, x5, x4, x3, x2, x1, x2 - x1}] === {x2 - x1}],
  VerificationTest[PossibleInvalidSubsets[{x7, x6, x5, x4, 1-x3, x2, x1, x2 - x1}] === {1 - x3, x2 - x1}]
}];

PrintTestResults[possibleInvalidSubsets];

Print["SubstitutionRangeVar"]
substitutionRangeVar = TestReport[{
  VerificationTest[SubstitutionRangeVar[{x1}] === {0,1}],
  VerificationTest[SubstitutionRangeVar[{x1+x2}] === {0,1,2,3}],
  VerificationTest[SubstitutionRangeVar[{1 - x1}] === {0,1}]
}];
PrintTestResults[substitutionRangeVar];

Print["ExpandVar"]
(*How to test (##) &[]?*)
expandVar = TestReport[{
  VerificationTest[ExpandVar[x2 + x1, 0] === (##) &[]],
  VerificationTest[ExpandVar[x2 + x1, 1] === (##) &[]],
  VerificationTest[ExpandVar[x2 + x1, 2] === (##) &[]],
  VerificationTest[ExpandVar[x2 + x1, 3] === {x1 -> 1, x2 -> 1}],
  VerificationTest[ExpandVar[1-x1, 1] === {}]
}];
PrintTestResults[expandVar];


Print["InvalidSubSets"]
invalidSubSets = TestReport[{
  VerificationTest[InvalidSubSets[{x7, x6, x5, x4, x3, x2, x1, x0}] === {}],
  VerificationTest[InvalidSubSets[{x7, x6, x5, x4, x3, x2, x1, x2 - x1}] === {{x1 -> 1, x2 -> 0}}],
  VerificationTest[InvalidSubSets[{x7, x6, x5, x4, x3, x2, 1 - x1, x2 - x1}] === {{x1 -> 1, x2 -> 0}}],
  VerificationTest[InvalidSubSets[{x7, x6, x5, x4, x3, x2, x2 + x3, x2 - x1}] === {{x2 -> 1, x3 -> 1}, {x1 -> 1, x2 -> 0}}]
}];
PrintTestResults[invalidSubSets];

Print["ExceptionTemplates"]
exceptionTemplatesReport = TestReport[{
  VerificationTest[ExceptionTemplates[{0, 0, 0, 0, 0, 0, 0, 0, 0}] === {}],
  VerificationTest[ExceptionTemplates[{x7, x6, x5, x4, x3, x2, x1, x0}] === {}],
  VerificationTest[ExceptionTemplates[{x7, x6, x5, x4, x3, x2, x1, x2 - x1}] === {{x7, x6, x5, x4, x3, 0, 1, x0}}],
  VerificationTest[ExceptionTemplates[{x7, x6, x5, x4, x3, x2, 1 - x1, x2 - x1}] === {{x7, x6, x5, x4, x3, 0, 1, x0}}],
  VerificationTest[ExceptionTemplates[{x7, x6, x5, x4, x3, x2, x2 + x3, x2 - x1}] === {{x7, x6, x5, x4, x3, 0, 1, x0}, {x7, x6, x5, x4, 1, 1, x1, x0}}],
  VerificationTest[ExceptionTemplates[{x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x2 - x1}, 2, 2.0] === {{x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 1, x0}}]
}];

PrintTestResults[exceptionTemplatesReport];