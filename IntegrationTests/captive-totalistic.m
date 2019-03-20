<< CATemplates`;

report = TestReport[{
  VerificationTest[
    With[
      {t31 = TotalisticTemplate[3, 1.0], c31 = CaptiveTemplate[3, 1.0]},
      Sort [FromDigits[#, 2] & /@
          ExpandTemplate[TemplateIntersection[t31, c31]]] === {223160992, 223161514, 265235104, 265235626}]]}];

PrintReport[report];