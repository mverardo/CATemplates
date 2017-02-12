(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 08/08/15 *)

<< CATemplates`;

SameOutputQ[group_] :=
    Equal @@ (group[[All, 2]])

TotalisticQ[karyTable_, k_, r_] :=
    With[{ruleTable = RuleTableFromKAry[karyTable, k, r]},
      With[{groups = GroupBy[ruleTable, Plus @@ (#[[1]]) &]},
        And @@ (SameOutputQ /@ (Values[groups]))]];

report = TestReport[{
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "core" -> {x7, x3, x3, x1, x3, x1, x1, x0}, "postExpansionFn" -> IdentityFn |>},
      TotalisticTemplate[2, 1.0] === expectedTemplate]],
  VerificationTest[
    (And @@ (TotalisticQ[#, 2, 1.0] & /@ ExpandTemplate[TotalisticTemplate[2, 1.0]])) === True],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 2.0, "core" -> {x31, x15, x15, x7, x15, x7, x7, x3, x15, x7, x7, x3, x7, x3, x3, x1, x15, x7, x7, x3, x7, x3, x3, x1, x7, x3, x3, x1, x3, x1, x1, x0}, "postExpansionFn" -> IdentityFn |>},
      TotalisticTemplate[2, 2.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.0, "core" -> {x26, x17, x8, x17, x8, x5, x8, x5, x2, x17, x8, x5, x8, x5, x2, x5, x2, x1, x8, x5, x2, x5, x2, x1, x2, x1, x0}, "postExpansionFn" -> IdentityFn |>},
      TotalisticTemplate[3, 1.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "core" -> {x7, x3, x5, x1, x3, x2, x1, x0}, "postExpansionFn" -> IdentityFn |>},
      OuterTotalisticTemplate[2, 1.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 2, "r" -> 2.0, "core" -> {x31, x15, x15, x7, x27, x11, x11, x3, x15, x7, x7, x5, x11, x3, x3, x1, x15, x7, x7, x5, x11, x3, x3, x1, x7, x5, x5, x4, x3, x1, x1, x0}, "postExpansionFn" -> IdentityFn |>},
      OuterTotalisticTemplate[2, 2.0] === expectedTemplate]],
  VerificationTest[
    With[
      {expectedTemplate = <|"k" -> 3, "r" -> 1.0, "core" -> {x26, x17, x8, x23, x14, x5, x20, x11, x2, x17, x8, x7, x14, x5, x4, x11, x2, x1, x8, x7, x6, x5, x4, x3, x2, x1, x0}, "postExpansionFn" -> IdentityFn |>},
      OuterTotalisticTemplate[3, 1.0] === expectedTemplate]]}];

PrintReport[report];
