(* ::Package:: *)

<< CATemplates`;

report = TestReport[
  {
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        template2 = BuildTemplate[2, 1., ConstantArray[0, 8], RawExpansion],
        result = {
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, x1, 1}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, 1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, 1, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, 1, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, 1, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, 1, x4, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, 1, x5, x4, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {1, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]}
      },
        TemplateDifference[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., ConstantArray[0, 8], RawExpansion],
        template2 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        result = {}
      },
        TemplateDifference[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        template2 = BuildTemplate[2, 1., ConstantArray[1, 8], RawExpansion],
        result = {
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, x1, 0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, 0, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, 0, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, 0, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, 0, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, 0, x4, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, 0, x5, x4, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {0, x6, x5, x4, x3, x2, x1, x0}, RawExpansion]}
      },
        TemplateDifference[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        result = {}
      },
        TemplateDifference[template1, template1] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 2., OldBaseTemplate[], RawExpansion],
        result = {}
      },
        TemplateDifference[template1, template1] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 2., {x7, 0, x5, 0, x3, 0, x1, 0}, RawExpansion],
        result = {}},
        TemplateDifference[template1, template1] === {}]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        template2 = BuildTemplate[2, 1., {x7, 0, x5, 0, x3, 0, x1, 0}, RawExpansion],
        result = {
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, x1, 1}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, 1, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, x6, x5, 1, x3, x2, x1, x0}, RawExpansion],
          BuildTemplate[2, 1., {x7, 1, x5, x4, x3, x2, x1, x0}, RawExpansion]}
      },
        TemplateDifference[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., {x7, 0, x5, 0, x3, 0, x1, 0}, RawExpansion],
        template2 = BuildTemplate[2, 1., OldBaseTemplate[], RawExpansion],
        result = {}},
        TemplateDifference[template1, template2] === result]
    ]
  }];

PrintTestResults[report];