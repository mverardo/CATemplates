<< CATemplates`


report = TestReport[
  {
    VerificationTest[
      ExpandTemplate[BuildTemplate[2, 1.0, {0, 1, x1 \[Element] {0, 2}, 1, 0}, RestrictedExpansion]] === {{0, 1, 0, 1, 0}}],
    VerificationTest[
      ExpandTemplate[BuildTemplate[2, 1.0, {0, 1, x1 \[Element] {0, 1}, 1, 0}, RestrictedExpansion]] === {{0, 1, 0, 1, 0}, {0, 1, 1, 1, 0}}],
    VerificationTest[
      ExpandTemplate[BuildTemplate[2, 1.0, {0, x1, x2 \[Element] {0, 1}, 2 - x2}, RestrictedExpansion]] === {{0, 0, 1, 1}, {0, 1, 1, 1}}],
    VerificationTest[
      ExpandTemplate[BuildTemplate[3, 1.0, {0, 2, x1 \[Element] {0, 2}, 0, 1}, RestrictedExpansion]] === {{0, 2, 0, 0, 1}, {0, 2, 2, 0, 1}}],
    VerificationTest[
      ExpandTemplate[BuildTemplate[3, 1.0, {0, 2, x1 \[Element] {1, 2}, 0, 1}, RestrictedExpansion]] === {{0, 2, 1, 0, 1}, {0, 2, 2, 0, 1}}],
    VerificationTest[
      ExpandTemplate[BuildTemplate[3, 1.0, {0, 2, x1 \[Element] {0, 1}, 0, 1}, RestrictedExpansion]] === {{0, 2, 0, 0, 1}, {0, 2, 1, 0, 1}}]
  }];

PrintTestResults[report];
