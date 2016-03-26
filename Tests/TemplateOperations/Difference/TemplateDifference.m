(* ::Package:: *)

<< CATemplates`;

report = TestReport[
  {
    VerificationTest[
      With[{
        k = 2,
        r = 1.,
        t1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        t2 = BuildTemplate[2, 1., ConstantArray[0, 8], RawExpansion],
        result = {<|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, x2, x1, 1}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, x2, 1, x0}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, 1, x1, x0}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, 1, x2, x1, x0}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, 1, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, 1, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, 1, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>, <|"k" -> 2, "r" -> 1.`, "rawList" -> {1, x6, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>}
      },
        TemplateDifference[t1, t2] === result]
    ],
    VerificationTest[
      With[{
        t2 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        t1 = BuildTemplate[2, 1., ConstantArray[0, 8], RawExpansion],
        result = {}
      },
        TemplateDifference[t1, t2] === result]
    ],
    VerificationTest[
      With[{
        t1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        t2 = BuildTemplate[2, 1., ConstantArray[1, 8], RawExpansion],
        result = {
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, x2, x1, 0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, x2, 0, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, 0, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, 0, x2, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, 0, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, 0, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, 0, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {0, x6, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>}
      },
        TemplateDifference[t1, t2] === result]
    ],
    VerificationTest[
      With[{
        t1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion]},
        TemplateDifference[t1, t1] === {}]
    ],
    VerificationTest[
      With[{
        t1 = BuildTemplate[2, 2., OldBaseTemplate[], RawExpansion]},
        TemplateDifference[t1, t1] === {}]
    ],
    VerificationTest[
      With[{
        t1 = BuildTemplate[2, 2., {x7, 0, x5, 0, x3, 0, x1, 0}, RawExpansion],
        result = {}},
        TemplateDifference[t1, t1] === {}]
    ],
    VerificationTest[
      With[{
        t1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        t2 = BuildTemplate[2, 1., {x7, 0, x5, 0, x3, 0, x1, 0}, RawExpansion],
        result = {
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, x2, x1, 1}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, x4, x3, 1, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, x6, x5, 1, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>,
          <|"k" -> 2, "r" -> 1.`, "rawList" -> {x7, 1, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>}
      },
        TemplateDifference[t1, t2] === result]
    ],
    VerificationTest[
      With[{
        t1 = BuildTemplate[2, 1., {x7, 0, x5, 0, x3, 0, x1, 0}, RawExpansion],
        t2 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        result = {}},
        TemplateDifference[t1, t2] === result]
    ]
  }];

Print["Suceeded: " <> ToString[report["TestsSucceededCount"]]];
Print["Failed: " <> ToString[report["TestsFailedCount"]]];
If[report["TestsFailedCount"] != 0,
  Print["Indices:" <> ToString[report["TestsFailedIndices"]]]
];