BeginPackage["CATemplates`CA`"];

RuleTableQ::usage = "Finds out if expression has a rule table form. Eg: {{{1, 1, 1}, 1}, {{1, 1, 0}, 1}, {{1, 0, 1}, 1}, {{1, 0, 0}, 1}, {{0, 1, 1}, 1}, {{0, 1, 0}, 1}, {{0, 0, 1}, 1}, {{0, 0, 0}, 1}}";
RuleQ::usage = "Finds out if expression has a rule form. Eg: {{0,1,1}, 1}";
NeighborhoodQ::usage = "Finds out if expression has a neighborhood form. Eg: {0,1,1}";

AllNeighbourhoods::usage = "Creates all possible  k^(2r+1)neighbourhoods of a given space.";
RuleTable::usage = "Creates the rule table of a rule by its index. ";

BWTransform::usage = "Performs the Black-White transform in a rule table, transition, neighbourhood or transition output.";
LRTransform::usage = "Performs the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
BWLRTransform::usage = "Performs the Black-White transform followed by the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
LRBWTransform::usage = "Performs the Left-Right transform followed by the Black-White transform in a rule table, transition, neighbourhood or transition output.";

RuleTableFromKAry::usage= "Converts a k-ary rule table to its classical representation.";
KAryFromRuleTable::usage = "Converts a rule table to its k-ary representation.";
RuleOutputFromNeighbourhood::usage="Yields the output bit of a given neighbourhood from rule rnum. The neighbourhood may be given as the k-ary sequence that defines it, or as the decimal number it represents (e.g, decimal 6 for neighbourhood {0, 1, 1, 0}, etc).";

Begin["`Private`"];

(* Expression matchers *)

RuleTableQ[expression_] :=
    MatchQ[expression, {{x_List, y_}, __}];

RuleQ[expression_] :=
    MatchQ[expression, {x_List, y_}];

NeighborhoodQ[expression_] :=
    MatchQ[expression, x_List];

(* Generators *)

AllNeighbourhoods[k_Integer : 2, r_ : 1] :=
    Tuples[Range[k - 1, 0, -1], Floor[2 r + 1]];

RuleTable[rnum_Integer, k_Integer: 2, r_: 1] :=
    RuleTableFromKAry[IntegerDigits[rnum, k, Power[k, Floor[2 r + 1]]], k, r];

(* Adapters *)

RuleTableFromKAry[kAryRuleTable_, k_Integer: 2, r_: 1] :=
    MapThread[List[#1, #2] &, {Tuples[Range[k - 1, 0, -1], Floor[2 r + 1]], kAryRuleTable}];

KAryFromRuleTable[ruleTable_] :=
    #[[2]] & /@ ruleTable;

RuleOutputFromNeighbourhood[neighbourhood_List, rnum_Integer, k_Integer: 2, r_: 1] :=
    RuleOutputFromNeighbourhood[FromDigits[neighbourhood, k], KAryFromRuleTable[RuleTable[rnum, k, r]], k, r];

RuleOutputFromNeighbourhood[neighbourhood_List, kAryRuleTable_List, k_Integer: 2, r_: 1] :=
    RuleOutputFromNeighbourhood[FromDigits[neighbourhood, k], kAryRuleTable, k, r];

RuleOutputFromNeighbourhood[neighbourhoodindex_Integer, rnum_Integer, k_Integer: 2, r_: 1] :=
    RuleOutputFromNeighbourhood[neighbourhoodindex, KAryFromRuleTable[RuleTable[rnum, k, r]], k, r];

RuleOutputFromNeighbourhood[neighbourhoodindex_Integer, kAryRuleTable_List, k_Integer: 2, r_: 1] :=
    Extract[kAryRuleTable, {Power[k, Floor[2 r + 1]] - neighbourhoodindex}];

(* Transformations *)

BWTransform[ruleTable_ /; RuleTableQ[ruleTable]] :=
    Reverse[SortBy[BWTransform /@ ruleTable, First]];

BWTransform[rule_] :=
    1 - rule;

LRTransform[ruleTable_ /; RuleTableQ[ruleTable]] :=
    Reverse[SortBy[LRTransform /@ ruleTable, First]];

LRTransform[rule_ /; RuleQ[rule]] :=
    {Reverse [rule[[1]]], rule[[2]]};

LRTransform[neighborhood_ /; NeighborhoodQ[neighborhood]] :=
    Reverse[neighborhood];

LRTransform[neighborhood_] :=
    neighborhood;

BWLRTransform := Composition[LRTransform, BWTransform];

LRBWTransform := Composition[BWTransform, LRTransform];

End[];

EndPackage[];