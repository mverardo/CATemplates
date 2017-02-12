(* ::Package:: *)

<< CATemplates`;

ColorBlindForPermutationQ[karyTable_, permutation_, k_] :=
    Module[
      {ruleTable},
      ruleTable = RuleTableFromKAry[karyTable, k];
      Reverse[Sort[(ruleTable /. permutation)]] == ruleTable
    ];

ColorBlindQ[ruleTable_, k_] :=
    And @@ (ColorBlindForPermutationQ[ruleTable, #, k] & /@ PossiblePermutations[k]);

report = TestReport[{
  VerificationTest[templateCore[ColorBlindTemplate[2]] === templateCore[SymmetricTemplate[BWTransform, 8][[1]]]],
  VerificationTest[And @@ (ColorBlindQ[#, 3] & /@ ExpandTemplate[ColorBlindTemplate[3]]) === True]
}];

PrintReport[report];
