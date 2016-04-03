<< CATemplates`

report = TestReport[{
  VerificationTest[
    With[{
      t1 = OldBaseTemplate[],
      t2 = ConstantArray[0, 8]},
      RawIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = OldBaseTemplate[],
      t2 = ConstantArray[1, 8]},
      RawIntersection[t1, t2] === t2]],
  VerificationTest[
    With[{
      t1 = OldBaseTemplate[]},
      RawIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = OldBaseTemplate[2, 2]},
      RawIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = OldBaseTemplate[2, 3]},
      RawIntersection[t1, t1] === t1]],
  VerificationTest[
    With[{
      t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
      t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
      result = {x7, 0, x5, 0, x3, 0, x1, 0}},
      RawIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = OldBaseTemplate[],
      t2 = {x7, 0, x5, 0, x3, 0, x1, 0},
      result = {x7, 0, x5, 0, x3, 0, x1, 0}},
      RawIntersection[t1, t2] === result]],
  VerificationTest[
    With[{
      t1 = {x7, 0, x5, 0, x3, 0, x1, 0},
      t2 = OldBaseTemplate[],
      result = {x7, 0, x5, 0, x3, 0, x1, 0}},
      RawIntersection[t1, t2] === result]]}];

PrintTestResults[report];