(* ::Package:: *)

<< CATemplates`;

totalT3  = <|"k" -> 2, "r" -> 3.,
  "rawList" -> {x127, x63, x63, x31, x63, x31, x31, x15, x63, x31, x31,
    x15, x31, x15, x15, x7, x63, x31, x31, x15, x31, x15, x15, x7,
    x31, x15, x15, x7, x15, x7, x7, x3, x63, x31, x31, x15, x31, x15,
    x15, x7, x31, x15, x15, x7, x15, x7, x7, x3, x31, x15, x15, x7,
    x15, x7, x7, x3, x15, x7, x7, x3, x7, x3, x3, x1, x63, x31, x31,
    x15, x31, x15, x15, x7, x31, x15, x15, x7, x15, x7, x7, x3, x31,
    x15, x15, x7, x15, x7, x7, x3, x15, x7, x7, x3, x7, x3, x3, x1,
    x31, x15, x15, x7, x15, x7, x7, x3, x15, x7, x7, x3, x7, x3, x3,
    x1, x15, x7, x7, x3, x7, x3, x3, x1, x7, x3, x3, x1, x3, x1, x1,
    x0}, "expansionFunction" -> FilteredExpansion|>;
totalR3  = ExpandTemplate[totalT3];
outTotT3 = <|"k" -> 2, "r" -> 3.,
  "rawList" -> {x127, x63, x63, x31, x63, x31, x31, x15, x119, x55,
    x55, x23, x55, x23, x23, x7, x63, x31, x31, x15, x31, x15, x15,
    x11, x55, x23, x23, x7, x23, x7, x7, x3, x63, x31, x31, x15, x31,
    x15, x15, x11, x55, x23, x23, x7, x23, x7, x7, x3, x31, x15, x15,
    x11, x15, x11, x11, x9, x23, x7, x7, x3, x7, x3, x3, x1, x63, x31,
    x31, x15, x31, x15, x15, x11, x55, x23, x23, x7, x23, x7, x7, x3,
    x31, x15, x15, x11, x15, x11, x11, x9, x23, x7, x7, x3, x7, x3, x3,
    x1, x31, x15, x15, x11, x15, x11, x11, x9, x23, x7, x7, x3, x7,
    x3, x3, x1, x15, x11, x11, x9, x11, x9, x9, x8, x7, x3, x3, x1, x3,
    x1, x1, x0}, "expansionFunction" -> FilteredExpansion|>;
outTotR3  = ExpandTemplate[outTotT3];

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
        diffa = TemplateDifference[t1, t2];
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
        diffa = TemplateDifference[t1, t2];
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
        diffa = TemplateDifference[t1, t2];
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
        diffa = TemplateDifference[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expected = Complement[r1,r2];
        expectedr = Sort[FromDigits[#, 2] & /@ expected];
        ContainsExactly[fra,expectedr]]]
  }];
PrintTestResults[stateCTDiffTest2];