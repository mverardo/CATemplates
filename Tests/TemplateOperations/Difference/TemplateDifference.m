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
        TemplateDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t2 = OldBaseTemplate[],
        t1 = ConstantArray[0, 8],
        result = {}
      },
        TemplateDifference[t1, t2] === result]],
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
        TemplateDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[]},
        TemplateDifference[t1, t1] === {}]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[2, 2]},
        TemplateDifference[t1, t1] === {}]],
    VerificationTest[
      With[{
        t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
        t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
        result = {x7, 0, x5, 0, x3, 0, x1, 0}},
        TemplateDifference[t1, t2] === {}]],
    VerificationTest[
      With[{
        t1 = OldBaseTemplate[],
        t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
        result = {{x7, x6, x5, x4, x3, x2, x1, 1}, {x7, x6, x5, x4, x3, 1, x1,
          x0}, {x7, x6, x5, 1, x3, x2, x1, x0}, {x7, 1, x5, x4, x3, x2, x1,
          x0}}
      },
        TemplateDifference[t1, t2] === result]],
    VerificationTest[
      With[{
        t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
        t2 = OldBaseTemplate[],
        result = {}},
        TemplateDifference[t1, t2] === result]]
  }];

Print["Suceeded: " <> ToString[report["TestsSucceededCount"]]];
Print["Failed: " <> ToString[report["TestsFailedCount"]]];
If[report["TestsFailedCount"]!=0,
  Print["Indices:" <> ToString[report["TestsFailedIndices"]]]
];