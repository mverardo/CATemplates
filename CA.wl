BeginPackage["CATemplates`CA`"];

RuleTableQ::usage = "Finds out if expression has a rule table form. Eg: {{{1, 1, 1}, 1}, {{1, 1, 0}, 1}, {{1, 0, 1}, 1}, {{1, 0, 0}, 1}, {{0, 1, 1}, 1}, {{0, 1, 0}, 1}, {{0, 0, 1}, 1}, {{0, 0, 0}, 1}}";
RuleQ::usage = "Finds out if expression has a rule form. Eg: {{0,1,1}, 1}";
NeighborhoodQ::usage = "Finds out if expression has a neighborhood form. Eg: {0,1,1}";
AllNeighbourhoods::usage = "Creates all possible  k^(2r+1)neighbourhoods of a given space.";
BWTransform::usage = "Performs the Black-White transform in a rule table, transition, neighbourhood or transition output.";
LRTransform::usage = "Performs the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
BWLRTransform::usage = "Performs the Black-White transform followed by the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
LRBWTransform::usage = "Performs the Left-Right transform followed by the Black-White transform in a rule table, transition, neighbourhood or transition output.";

Begin["`Private`"];

RuleTableQ[expression_] :=
    MatchQ[expression, {{x_List, y_}, __}];

RuleQ[expression_] :=
    MatchQ[expression, {x_List, y_}];

NeighborhoodQ[expression_] :=
    MatchQ[expression, x_List];

AllNeighbourhoods[k_Integer : 2, r_ : 1] :=
    Tuples[Range[k - 1, 0, -1], Floor[2 r + 1]];

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