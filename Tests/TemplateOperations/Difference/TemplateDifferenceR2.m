(* ::Package:: *)

<< CATemplates`;

colorBT2 = <|"k" -> 2, "r" -> 2.,
  "rawList" -> {1 - x0, 1 - x1, 1 - x2, 1 - x3, 1 - x4, 1 - x5, 1 - x6,
    1 - x7, 1 - x8, 1 - x9, 1 - x10, 1 - x11, 1 - x12, 1 - x13,
    1 - x14, 1 - x15, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5,
    x4, x3, x2, x1, x0}, "expansionFunction" -> FilteredExpansion|>;
colorBR2  = ExpandTemplate[colorBT2];
stateCT2 = <|"k" -> 2, "r" -> 2.,
  "rawList" -> {x31, x15, x15, x7, x15, x7, x7, x3, x15, x7, x7, x3,
    x7, x3, x3, x1, x15, x7, x7, x3, x7, x3, x3, x1, x7, x3, x3, x1,
    x3, x1, x1, x0}, "expansionFunction" -> FilteredExpansion|>;
stateCR2 = ExpandTemplate[stateCT2];
totalT2  = <|"k"->2,"r"->2.`,"rawList"->{x31,x15,x15,x7,x15,x7,x7,x3,x15,x7,x7,x3,x7,x3,x3,x1,x15,x7,x7,x3,x7,x3,x3,x1,x7,x3,x3,x1,x3,x1,x1,x0},"expansionFunction"->FilteredExpansion|>;
totalR2  = ExpandTemplate[totalT2];
outTotT2 = <|"k"->2,"r"->2.`,"rawList"->{x31,x15,x15,x7,x27,x11,x11,x3,x15,x7,x7,x5,x11,x3,x3,x1,x15,x7,x7,x5,x11,x3,x3,x1,x7,x5,x5,x4,x3,x1,x1,x0},"expansionFunction"->FilteredExpansion|>;
outTotR2  = ExpandTemplate[outTotT2];

(*Print["ColorBlindTemplate DiffTest2"];*)
(*colorBTDiffTest2 = TestReport[*)
  (*{*)
    (*VerificationTest[*)
      (*With[{*)
        (*t1 = colorBT2,*)
        (*t2 = colorBT2*)
      (*},*)
        (*diffa = TemplateDifferenceDirect[t1, t2];*)
        (*ra = ExpandTemplate /@ diffa;*)
        (*fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];*)
        (*expectedr = {};*)
        (*ContainsExactly[fra,expectedr]]],*)
    (*VerificationTest[*)
      (*With[{*)
        (*t1 = colorBT2,*)
        (*t2 = stateCT2,*)
        (*r1 = colorBR2,*)
        (*r2 = stateCR2*)
      (*},*)
        (*diffa = TemplateDifferenceDirect[t1, t2];*)
        (*ra = ExpandTemplate /@ diffa;*)
        (*fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];*)
        (*expected = Complement[r1,r2];*)
        (*expectedr = Sort[FromDigits[#, 2] & /@ expected];*)
        (*ContainsExactly[fra,expectedr]]],*)
    (*VerificationTest[*)
      (*With[{*)
        (*t1 = colorBT2,*)
        (*t2 = totalT2,*)
        (*r1 = colorBR2,*)
        (*r2 = totalR2*)
      (*},*)
        (*diffa = TemplateDifferenceDirect[t1, t2];*)
        (*ra = ExpandTemplate /@ diffa;*)
        (*fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];*)
        (*expected = Complement[r1,r2];*)
        (*expectedr = Sort[FromDigits[#, 2] & /@ expected];*)
        (*ContainsExactly[fra,expectedr]]],*)
    (*VerificationTest[*)
      (*With[{*)
        (*t1 = colorBT2,*)
        (*t2 = outTotT2,*)
        (*r1 = colorBR2,*)
        (*r2 = outTotR2*)
      (*},*)
        (*diffa = TemplateDifferenceDirect[t1, t2];*)
        (*ra = ExpandTemplate /@ diffa;*)
        (*fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];*)
        (*expected = Complement[r1,r2];*)
        (*expectedr = Sort[FromDigits[#, 2] & /@ expected];*)
        (*ContainsExactly[fra,expectedr]]]*)
  (*}];*)
(*PrintTestResults[colorBTDiffTest2];*)

Print["StateConservingTemplate DiffTest2"];
stateCTDiffTest2 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = stateCT2,
        t2 = colorBT2,
        r1 = stateCR2,
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
        t1 = stateCT2,
        t2 = stateCT2,
        r1 = stateCR2,
        r2 = stateCR2
      },
        diffa = TemplateDifferenceDirect[t1, t2];
        ra = ExpandTemplate /@ diffa;
        fra = Sort[FromDigits[#, 2] & /@DeleteDuplicates[Join@@ra]];
        expectedr = {};
        ContainsExactly[fra,expectedr]]],
    VerificationTest[
      With[{
        t1 = stateCT2,
        t2 = totalT2,
        r1 = stateCR2,
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
        t1 = stateCT2,
        t2 = outTotT2,
        r1 = stateCR2,
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
        t2 = stateCT2,
        r1 = totalR2,
        r2 = stateCR2
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
        t2 = stateCT2,
        r1 = outTotR2,
        r2 = stateCR2
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