(* ::Package:: *)

<< CATemplates`;

report = TestReport[
  {
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = ConstantArray[0, 8],
        result = {{x7, x6, x5, x4, x3, x2, x1, 1}, {x7, x6, x5, x4, x3, x2, 1,
          x0}, {x7, x6, x5, x4, x3, 1, x1, x0}, {x7, x6, x5, x4, 1, x2, x1,
          x0}, {x7, x6, x5, 1, x3, x2, x1, x0}, {x7, x6, 1, x4, x3, x2, x1,
          x0}, {x7, 1, x5, x4, x3, x2, x1, x0}, {1, x6, x5, x4, x3, x2, x1,
          x0}}
      },
        RawDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t2 = OldBaseTemplate[],
        t1 = ConstantArray[0, 8],
        result = {}
      },
        RawDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = ConstantArray[1, 8],
        result = {{x7, x6, x5, x4, x3, x2, x1, 0}, {x7, x6, x5, x4, x3, x2, 0,
          x0}, {x7, x6, x5, x4, x3, 0, x1, x0}, {x7, x6, x5, x4, 0, x2, x1,
          x0}, {x7, x6, x5, 0, x3, x2, x1, x0}, {x7, x6, 0, x4, x3, x2, x1,
          x0}, {x7, 0, x5, x4, x3, x2, x1, x0}, {0, x6, x5, x4, x3, x2, x1,
          x0}}
      },
        RawDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[]},
        RawDifference[t1, t1] === {}]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[2, 2]},
        RawDifference[t1, t1] === {}]],
    VerificationTest[
      With[{
        t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
        t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
        result = {x7, 0, x5, 0, x3, 0, x1, 0}},
        RawDifference[t1, t2] === {}]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
        result = {{x7, x6, x5, x4, x3, x2, x1, 1}, {x7, x6, x5, x4, x3, 1, x1,
          x0}, {x7, x6, x5, 1, x3, x2, x1, x0}, {x7, 1, x5, x4, x3, x2, x1,
          x0}}
      },
        RawDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
        t2 = OldBaseTemplate[],
        result = {}},
        RawDifference[t1, t2] === result]]
  }];

PrintTestResults[report];

report2 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = ConstantArray[0, 8],
        result = {{x7, x6, x5, x4, x3, x2, x1, 1}, {x7, x6, x5, x4, x3, x2, 1,
          x0}, {x7, x6, x5, x4, x3, 1, x1, x0}, {x7, x6, x5, x4, 1, x2, x1,
          x0}, {x7, x6, x5, 1, x3, x2, x1, x0}, {x7, x6, 1, x4, x3, x2, x1,
          x0}, {x7, 1, x5, x4, x3, x2, x1, x0}, {1, x6, x5, x4, x3, x2, x1,
          x0}}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t2 = OldBaseTemplate[],
        t1 = ConstantArray[0, 8],
        result = {}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = ConstantArray[1, 8],
        result = {{x7, x6, x5, x4, x3, x2, x1, 0}, {x7, x6, x5, x4, x3, x2, 0,
          x0}, {x7, x6, x5, x4, x3, 0, x1, x0}, {x7, x6, x5, x4, 0, x2, x1,
          x0}, {x7, x6, x5, 0, x3, x2, x1, x0}, {x7, x6, 0, x4, x3, x2, x1,
          x0}, {x7, 0, x5, x4, x3, x2, x1, x0}, {0, x6, x5, x4, x3, x2, x1,
          x0}}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[]},
        diffa = RawDifference[t1, t1];
        diffb = RawDifferenceB[t1, t1];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[2, 2]},
        diffa = RawDifference[t1, t1];
        diffb = RawDifferenceB[t1, t1];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
        t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
        result = {x7, 0, x5, 0, x3, 0, x1, 0}},
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
        result = {{x7, x6, x5, x4, x3, x2, x1, 1}, {x7, x6, x5, x4, x3, 1, x1,
          x0}, {x7, x6, x5, 1, x3, x2, x1, x0}, {x7, 1, x5, x4, x3, x2, x1,
          x0}}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
        t2 = OldBaseTemplate[],
        result = {}},
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]
  }];

PrintTestResults[report2];


captT   = <|"k" -> 2, "r" -> 1., "rawList" -> {1, x6, x5, x4, x3, x2, x1, 0}, "expansionFunction" -> RawExpansion|>;
colorBT = <|"k" -> 2, "r" -> 1., "rawList" -> {1 - x0, 1 - x1, 1 - x2, 1 - x3, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>;
stateCT = <|"k" -> 2, "r" -> 1., "rawList" -> {1, 1 + x2 - x3, 1 - x2, 1 - x1 - x2, x3, x2, x1, 0}, "expansionFunction" -> RawExpansion|>;
totalT  = <|"k" -> 2, "r" -> 1., "rawList" -> {x7, x3, x3, x1, x3, x1, x1, x0}, "expansionFunction" -> RawExpansion|>;
outTotT = <|"k" -> 2, "r" -> 1., "rawList" -> {x7, x3, x5, x1, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>;
baseT   = <|"k" -> 2, "r" -> 1., "rawList" -> {x7, x6, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> RawExpansion|>;
(*captT   = CaptiveTemplate[];*)
(*colorBT = ColorBlindTemplate[];*)
(*stateCT = StateConservingTemplate[];*)
(*totalT  = TotalisticTemplate[];*)
(*outTotT = OuterTotalisticTemplate[];*)

Print["CaptiveTemplate DiffTest"];
captTDiffTest = TestReport[
  {
    VerificationTest[
      With[{
        t1 = captT[["rawList"]],
        t2 = captT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = captT[["rawList"]],
        t2 = colorBT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = captT[["rawList"]],
        t2 = stateCT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = captT[["rawList"]],
        t2 = totalT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = captT[["rawList"]],
        t2 = outTotT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = captT,
        t2 = baseT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]
    ]
  }];
PrintTestResults[captTDiffTest];

Print["ColorBlindTemplate DiffTest"];
colorBTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = colorBT[["rawList"]],
        t2 = captT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = colorBT[["rawList"]],
        t2 = colorBT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = colorBT[["rawList"]],
        t2 = stateCT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = colorBT[["rawList"]],
        t2 = totalT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];


        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = colorBT[["rawList"]],
        t2 = outTotT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = colorBT[["rawList"]],
        t2 = baseT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]
  }];
PrintTestResults[colorBTDiffTest];

Print["StateConservingTemplate DiffTest"];
stateCTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = stateCT[["rawList"]],
        t2 = captT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = stateCT[["rawList"]],
        t2 = colorBT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = stateCT[["rawList"]],
        t2 = stateCT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = stateCT[["rawList"]],
        t2 = totalT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = stateCT[["rawList"]],
        t2 = outTotT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = stateCT[["rawList"]],
        t2 = baseT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]
  }];
PrintTestResults[stateCTDiffTest];

Print["TotalisticTemplate DiffTest"];
totalTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = totalT[["rawList"]],
        t2 = captT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = totalT[["rawList"]],
        t2 = colorBT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = totalT[["rawList"]],
        t2 = stateCT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = totalT[["rawList"]],
        t2 = totalT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = totalT[["rawList"]],
        t2 = outTotT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = totalT[["rawList"]],
        t2 = baseT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]
  }];
PrintTestResults[totalTDiffTest];

Print["OuterTotalisticTemplate DiffTest"];
outTotTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = outTotT[["rawList"]],
        t2 = captT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = outTotT[["rawList"]],
        t2 = colorBT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = outTotT[["rawList"]],
        t2 = stateCT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = outTotT[["rawList"]],
        t2 = totalT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = outTotT[["rawList"]],
        t2 = outTotT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = outTotT[["rawList"]],
        t2 = baseT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]
  }];
PrintTestResults[outTotTDiffTest];

Print["OldBaseTemplate DiffTest"];
baseTDiffTest = TestReport[
  {

    VerificationTest[
      With[{
        t1 = baseT,
        t2 = captT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = colorBT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = stateCT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = totalT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = outTotT[["rawList"]]
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = baseT,
        t2 = baseT
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]
  }];
PrintTestResults[baseTDiffTest];














report4 = TestReport[
  {
    VerificationTest[
      With[{
        t1 = {1,x2,x1,0},
        t2 = {1-x0,1-x1,x1,x0}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = {1-x0,1-x1,x1,x0},
        t2 = {1,x2,x1,0}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = {1,x0},
        t2 = {x1,0}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]],
    VerificationTest[
      With[{
        t1 = {1,x0},
        t2 = {0,x1}
      },
        diffa = RawDifference[t1, t2];
        diffb = RawDifferenceB[t1, t2];
        diffb === diffa]]

  }];

PrintTestResults[report4];