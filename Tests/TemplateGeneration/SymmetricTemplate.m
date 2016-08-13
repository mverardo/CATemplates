(* ::Package:: *)

<< CATemplates`;


EqualTransitionCount::usage =
    "Finds out the quantity of shared transitions between two k-ary rule tables.";
EqualTransitionCount[karyRuleTableA_, karyRuleTableB_] :=
    Plus @@ MapThread[If[#1 == #2, 1, 0] &, {karyRuleTableA, karyRuleTableB}]

SymmetryValues::usage =
    "Finds the internal symmetry values according to one of the transformations.";
SymmetryValues[transformation_, initRuleTables_List : {}] :=
    Module[{transformedRuleTables, both, bothValues, ruleTables},
      ruleTables = If[Length[initRuleTables] == 0, Tuples[{0, 1}, 8], initRuleTables];
      transformedRuleTables = KAryFromRuleTable[transformation[RuleTableFromKAry[#]]] & /@ ruleTables;
      both = Transpose[{ruleTables, transformedRuleTables}];
      bothValues = {#[[1]], #[[2]], EqualTransitionCount[#[[1]], #[[2]]]} & /@ both;
      bothValues]

RulesWithSymmetry::usage = "Finds the rules that have a given value for symmetry";
RulesWithSymmetry[transformations_List, value_] :=
    Module[{symmetryValues},
      symmetryValues = #[[1]] & /@ Select[SymmetryValues[#], (Last[#] == value &)] & /@ transformations;
      Apply[Intersection, symmetryValues]];

RulesWithSymmetry[transformation_, value_] := #[[1]] & /@
    Select[SymmetryValues[transformation], Last[#] == value &];


report = TestReport[{
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWTransform, 2])] === RulesWithSymmetry[BWTransform, 2]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWTransform, 4])] === RulesWithSymmetry[BWTransform, 4]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWTransform, 6])] === RulesWithSymmetry[BWTransform, 6]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWTransform, 8])] == RulesWithSymmetry[BWTransform, 8]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[LRTransform, 2])] == RulesWithSymmetry[LRTransform, 2]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[LRTransform, 4])] == RulesWithSymmetry[LRTransform, 4]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[LRTransform, 6])] == RulesWithSymmetry[LRTransform, 6]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[LRTransform, 8])] == RulesWithSymmetry[LRTransform, 8]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWLRTransform, 2])] == RulesWithSymmetry[BWLRTransform, 2]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWLRTransform, 4])] == RulesWithSymmetry[BWLRTransform, 4]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWLRTransform, 6])] == RulesWithSymmetry[BWLRTransform, 6]],
  VerificationTest[
    Sort[Union @@ (ExpandTemplate /@ SymmetricTemplate[BWLRTransform, 8])] == RulesWithSymmetry[BWLRTransform, 8]]}];

PrintReport[report];