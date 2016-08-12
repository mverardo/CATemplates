(* ::Package:: *)

<< CATemplates`;

colorBT2 = ColorBlindTemplate[2, 2.0];
colorBR2 = ExpandTemplate[colorBT2];
totalT2 = TotalisticTemplate[2, 2.0];
totalR2 = ExpandTemplate[totalT2];
outTotT2 = OuterTotalisticTemplate[2, 2.0];
outTotR2 = ExpandTemplate[outTotT2];

Print["ColorBlindTemplate DiffTest2"];
colorBTDiffTest2 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = colorBT2,
        t2 = colorBT2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expectedr = {};
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT2,
        t2 = totalT2,
        r1 = colorBR2,
        r2 = totalR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = colorBT2,
        t2 = outTotT2,
        r1 = colorBR2,
        r2 = outTotR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[colorBTDiffTest2];

Print["TotalisticTemplate DiffTest2"];
stateCTDiffTest2 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = totalT2,
        t2 = colorBT2,
        r1 = totalR2,
        r2 = colorBR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT2,
        t2 = totalT2,
        r1 = totalR2,
        r2 = totalR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT2,
        t2 = outTotT2,
        r1 = totalR2,
        r2 = outTotR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[stateCTDiffTest2];

Print["OuterTotalisticTemplate DiffTest2"];
stateCTDiffTest2 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = outTotT2,
        t2 = colorBT2,
        r1 = outTotR2,
        r2 = colorBR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT2,
        t2 = totalT2,
        r1 = outTotR2,
        r2 = totalR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT2,
        t2 = outTotT2,
        r1 = outTotR2,
        r2 = outTotR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[stateCTDiffTest2];