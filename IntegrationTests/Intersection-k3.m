<< CATemplates`;

TestCommutativity[t1_Association, t2_Association] :=
    TemplateIntersection[t1, t2] === TemplateIntersection[t2, t1];

report = TestReport[{
  VerificationTest[TestCommutativity[CaptiveTemplate[3,1.0], TotalisticTemplate[3, 1.0]]]}];

PrintReport[report];                                                                                                                                                                                                 