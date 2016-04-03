(* ::Package:: *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, RawExpansion]},
      ModularTemplateIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, RawExpansion],
      t2 = BuildTemplate[3, 1.0, {1, x0}, RawExpansion]},
      ModularTemplateIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x2, 1, x0}, RawExpansion],
      t2 = BuildTemplate[3, 1.0, {x2, x1, x0}, RawExpansion]},
      ModularTemplateIntersection[t1, t2] === t1]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, RawExpansion],
      t2 = BuildTemplate[3, 1.0, {1, x0}, RawExpansion],
      t3 = BuildTemplate[3, 1.0, {0, x0}, RawExpansion]},
      With[{i1 = ModularTemplateIntersection[t1, t2],
        i2 = ModularTemplateIntersection[t1, t3],
        result = BuildTemplate[3, 1.0, {}, RawExpansion]},
        ModularTemplateIntersection[i1, i2] === result]]],
  VerificationTest[
    With[{
      t1 = BuildTemplate[3, 1.0, {x1, x0}, RawExpansion],
      t2 = BuildTemplate[3, 1.0, {1, x0}, RawExpansion],
      t3 = BuildTemplate[3, 1.0, {x1, 0}, RawExpansion]},
      With[{
        i1 = ModularTemplateIntersection[t1, t2],
        i2 = ModularTemplateIntersection[t1, t3],
        result = BuildTemplate[3, 1.0, {1, 0}, RawExpansion]},
        ModularTemplateIntersection[i1, i2] === result]]]}];

PrintTestResults[report];
