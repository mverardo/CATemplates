(* ::Package:: *)

<< CATemplates`;

Print["BuildTemplate"];

report = TestReport[{
  VerificationTest[BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}, Identity] === <|"k" -> 2, "r" -> 1.0, "rawList" -> {x7, x6, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> Identity|>],
  VerificationTest[BuildTemplate[2, 1.0, {x7, 1, x5, x4, 1, x2, x1, 0}, Identity] === <|"k" -> 2, "r" -> 1.0, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Identity|>],
  VerificationTest[BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2]] === <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2]|>],
  VerificationTest[BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 2] === <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2], "N" -> 2|>],
  VerificationTest[BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 3] === <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2], "N" -> 3|>]
}];

PrintTestResults[report];

Print["BaseTemplate"];

report = TestReport[{
  VerificationTest[BaseTemplate[2, 1.0] === BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]],
  VerificationTest[BaseTemplate[2, 1.5] === BuildTemplate[2, 1.5, {x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]],
  VerificationTest[BaseTemplate[2, 2.0] === BuildTemplate[2, 2.0, {x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]]
}];

PrintTestResults[report];