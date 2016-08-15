(* ::Package:: *)

<< CATemplates`;

report = TestReport[
  {
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[]],
        template2 = BuildTemplate[2, 1., ConstantArray[0, 8]],
        result = {
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, x1, 1}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, 1, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, 1, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, 1, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, 1, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, 1, x4, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, 1, x5, x4, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {1, x6, x5, x4, x3, x2, x1, x0}]}
      },
        TemplateDifferenceDirect[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., ConstantArray[0, 8]],
        template2 = BuildTemplate[2, 1., OldBaseTemplate[]],
        result = {}
      },
        TemplateDifferenceDirect[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[]],
        template2 = BuildTemplate[2, 1., ConstantArray[1, 8]],
        result = {
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, x1, 0}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, 0, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, 0, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, 0, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, 0, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, 0, x4, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, 0, x5, x4, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {0, x6, x5, x4, x3, x2, x1, x0}]}
      },
        TemplateDifferenceDirect[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[]],
        result = {}
      },
        TemplateDifferenceDirect[template1, template1] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 2., OldBaseTemplate[]],
        result = {}
      },
        TemplateDifferenceDirect[template1, template1] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 2., {x7, 0, x5, 0, x3, 0, x1, 0}],
        result = {}},
        TemplateDifferenceDirect[template1, template1] === {}]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., OldBaseTemplate[]],
        template2 = BuildTemplate[2, 1., {x7, 0, x5, 0, x3, 0, x1, 0}],
        result = {
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, x2, x1, 1}],
          BuildTemplate[2, 1., {x7, x6, x5, x4, x3, 1, x1, x0}],
          BuildTemplate[2, 1., {x7, x6, x5, 1, x3, x2, x1, x0}],
          BuildTemplate[2, 1., {x7, 1, x5, x4, x3, x2, x1, x0}]}
      },
        TemplateDifferenceDirect[template1, template2] === result]
    ],
    VerificationTest[
      With[{
        template1 = BuildTemplate[2, 1., {x7, 0, x5, 0, x3, 0, x1, 0}],
        template2 = BuildTemplate[2, 1., OldBaseTemplate[]],
        result = {}},
        TemplateDifferenceDirect[template1, template2] === result]
    ]
  }];

PrintTestResults[report];



baseT   = BuildTemplate[2, 1.0, OldBaseTemplate[]];
captT   = CaptiveTemplate[];
colorBT = ColorBlindTemplate[];
stateCT = StateConservingTemplate[];
mStatCT = ModNStateConservingTemplate[];
totalT  = TotalisticTemplate[];
outTotT = OuterTotalisticTemplate[];

Print["CaptiveTemplate DiffTest"];
captTDiffTest = TestReport[
  {
    VerificationTest[
      With[{
        t1 = captT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]
      ]],
    VerificationTest[
      With[{
        t1 = captT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = captT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = captT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = captT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = captT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]
    ]]
  }];
PrintTestResults[captTDiffTest];

Print["ColorBlindTemplate DiffTest"];
colorBTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = colorBT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[colorBTDiffTest];

Print["StateConservingTemplate DiffTest"];
stateCTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = stateCT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = stateCT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = stateCT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = stateCT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = stateCT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = stateCT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[stateCTDiffTest];

Print["TotalisticTemplate DiffTest"];
totalTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = totalT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[totalTDiffTest];

Print["OuterTotalisticTemplate DiffTest"];
outTotTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = outTotT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[outTotTDiffTest];

Print["OldBaseTemplate DiffTest"];
baseTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = baseT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[baseTDiffTest];



Print["ModNStateConservingTemplate DiffTest"];
baseTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = mStatCT,
        t2 = captT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = mStatCT,
        t2 = colorBT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = mStatCT,
        t2 = stateCT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = mStatCT,
        t2 = totalT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = mStatCT,
        t2 = outTotT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = mStatCT,
        t2 = baseT
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        rules1 = ExpandTemplate[t1];
        rules2 = ExpandTemplate[t2];
        expected = Complement[rules1,rules2];
        expectedr=Sort[FromDigits[#, 2] & /@ expected];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[baseTDiffTest];