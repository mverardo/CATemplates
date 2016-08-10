(* ::Package:: *)

<< CATemplates`;

TestTable[karyTable_, permutation_, k_] :=
    Module[
      {ruleTable},
      ruleTable = RuleTableFromKAry[karyTable, k];
      Reverse[Sort[(ruleTable /. permutation)]] == ruleTable
    ];

TestAllPermutations[ruleTable_, k_] :=
    And @@ (TestTable[ruleTable, #, k] & /@ PossibleStateReplacements[k]);

report = TestReport[{
  VerificationTest[templateCore[ColorBlindTemplate[2]] === templateCore[SymmetricTemplate[BWTransform, 8][[1]]]],
  VerificationTest[And @@ (TestAllPermutations[#, 3] & /@ ExpandTemplate[ColorBlindTemplate[3]]) === True]
}];

PrintTestResults[report];
