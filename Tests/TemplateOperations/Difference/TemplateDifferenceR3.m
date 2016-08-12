(* ::Package:: *)

<< CATemplates`;

totalT3 = TotalisticTemplate[2, 3.0];
outTotT3 = OuterTotalisticTemplate[2, 3.0];
totalR3  = ExpandTemplate[totalT3];
outTotR3 = ExpandTemplate[outTotT3];

Print["TotalisticTemplate DiffTest3"];
stateCTDiffTest3 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = totalT3,
        t2 = totalT3,
        r1 = totalR3,
        r2 = totalR3
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = totalT3,
        t2 = outTotT3,
        r1 = totalR3,
        r2 = outTotR3
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[stateCTDiffTest3];

Print["OuterTotalisticTemplate DiffTest2"];
stateCTDiffTest2 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = outTotT3,
        t2 = totalT3,
        r1 = outTotR3,
        r2 = totalR3
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = outTotT3,
        t2 = outTotT3,
        r1 = outTotR3,
        r2 = outTotR3
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[stateCTDiffTest2];