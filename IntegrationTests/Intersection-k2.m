
<< CATemplates`;

TestIntersection[t1_Association, t2_Association] :=
    And[
      Sort[Intersection[ExpandTemplate[t1], ExpandTemplate[t2]]] === Sort[ExpandTemplate[TemplateIntersection[t1, t2]]],
      Sort[Intersection[ExpandTemplate[t1], ExpandTemplate[t2]]] === Sort[ExpandTemplate[TemplateIntersection[t2, t1]]]
    ];

report = TestReport[{
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], ColorBlindTemplate[2,1.0]]],
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], StateConservingTemplate[2,1.0]]],
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], First[SymmetricTemplate[BWTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], First[SymmetricTemplate[LRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], TotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[CaptiveTemplate[2,1.0], OuterTotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[ColorBlindTemplate[2,1.0], StateConservingTemplate[2,1.0]]],
  VerificationTest[TestIntersection[ColorBlindTemplate[2,1.0], First[SymmetricTemplate[BWTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[ColorBlindTemplate[2,1.0], First[SymmetricTemplate[LRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[ColorBlindTemplate[2,1.0], First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[ColorBlindTemplate[2,1.0], TotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[ColorBlindTemplate[2,1.0], OuterTotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[StateConservingTemplate[2,1.0], First[SymmetricTemplate[BWTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[StateConservingTemplate[2,1.0], First[SymmetricTemplate[LRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[StateConservingTemplate[2,1.0], First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[StateConservingTemplate[2,1.0], TotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[StateConservingTemplate[2,1.0], OuterTotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[BWTransform, 8, 2, 1.0]], First[SymmetricTemplate[LRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[BWTransform, 8, 2, 1.0]], First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[BWTransform, 8, 2, 1.0]], TotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[BWTransform, 8, 2, 1.0]], OuterTotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[LRTransform, 8, 2, 1.0]], First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[LRTransform, 8, 2, 1.0]], TotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[LRTransform, 8, 2, 1.0]], OuterTotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]], TotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[First[SymmetricTemplate[BWLRTransform, 8, 2, 1.0]], OuterTotalisticTemplate[2, 1.0]]],
  VerificationTest[TestIntersection[TotalisticTemplate[2, 1.0], OuterTotalisticTemplate[2, 1.0]]]}];

PrintReport[report];
