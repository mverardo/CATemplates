<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {x2, x0, x0},
      restrictions = {x2 \[Element] {0, 2}},
      result = {x2 \[Element] {0, 2}, x0, x0}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === result]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {x2, x0, x0},
      restrictions = {x2 \[Element] {0, 2}, x2 \[Element] {0, 1}},
      result = {0, x0, x0}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === result]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {x2, x0, x0},
      restrictions = {x1 \[Element] {0, 2}, x2 \[Element] {0, 1}},
      result = {x2 \[Element] {0, 1}, x0 \[Element] {0, 2}, x0 \[Element] {0, 2}}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === result]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {0, x0, x0},
      restrictions = {x2 \[Element] {0, 2}},
      result = {0, x0, x0}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === result]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {2, x0, x0},
      restrictions = {x2 \[Element] {2}},
      result = {2, x0, x0}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === result]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {0, x0, x0},
      restrictions = {x2 \[Element] {2}}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === {}]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {x2, x0, x0},
      restrictions = {x2 \[Element] {2, 3}, x2 \[Element] {1, 2, 3}},
      result = {x2 \[Element] {2, 3}, x0, x0}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === result]],
  VerificationTest[
    With[{
      t1 = {x2, x1, x0},
      t2 = {x2, x0, x0},
      restrictions = {x2 \[Element] {0, 2}, x1 \[Element] {0, 1}, x2 \[Element] {0, 1}, x0 \[Element] {1, 2}},
      result = {x2 \[Element] {2, 3}, x0, x0}},
      RestrictedTemplateIntersection[t1, t2, restrictions] === {0, 1, 1}]]
}];

PrintTestResults[report];