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

captT = CaptiveTemplate[];
colorBT = ColorBlindTemplate[];
stateCT = StateConservingTemplate[];

With[{
  t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
  t2 = OldBaseTemplate[],
  result = {}
},
  diffa = RawDifference[t1, t2];
  diffb = RawDifferenceB[t1, t2];
  Print[t1];
  Print[t2];
  Print[diffa];
  Print[diffb];
  Print[diffb === diffa];
]
With[{
  t2 = OldBaseTemplate[],
  t1 = ConstantArray[0, 8],
  result = {}
},
  diffa = RawDifference[t1, t2];
  diffb = RawDifferenceB[t1, t2];
  Print[diffa];
  Print[diffb];
  Print[diffb === diffa];
]

