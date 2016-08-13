(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`SymmetricTemplate`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`"
  }];

BWTransform::usage = "Performs the Black-White transform in a rule table, transition, neighbourhood or transition output.";
LRTransform::usage = "Performs the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
BWLRTransform::usage = "Performs the Black-White transform followed by the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
LRBWTransform::usage = "Performs the Left-Right transform followed by the Black-White transform in a rule table, transition, neighbourhood or transition output.";
SymmetricTemplate::usage="SymmetricTemplate[transform_, symmetryValue_Integer, k_Integer: 2, r_Real: 1.0, templateQuantity_: All] := Returns a set of templates representative of all the rules in a space given by k and r that share a symmetryValue according to transform.";


Begin["`Private`"];

RuleTableQ[expression_] :=
    MatchQ[expression, {{x_List, y_}, __}];

RuleQ[expression_] :=
    MatchQ[expression, {x_List, y_}];

NeighborhoodQ[expression_] :=
    MatchQ[expression, x_List];

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

SymmetricTemplate[transform_, symmetryValue_Integer, k_Integer: 2, r_Real: 1.0, templateQuantity_: All] :=
  Module[
    {neighbourhoods, neighbourhoodEquivalences, templateEquivalences, transformedEquivalences, equivalenceSubsets, unequalComplements, equations, inequations, systems, solutions,invariantNeighborhoodQuantity, templateLists},
    neighbourhoods = AllNeighbourhoods[k, r];
    neighbourhoodEquivalences = {#, transform[#]} & /@ neighbourhoods;
    invariantNeighborhoodQuantity = Length[Select[neighbourhoodEquivalences, #[[1]] == #[[2]] &]];
    neighbourhoodEquivalences = Select[neighbourhoodEquivalences, #[[1]] != #[[2]] &];
    If[symmetryValue < invariantNeighborhoodQuantity, 
      Return[{}]
    ];
    neighbourhoodEquivalences = Union[Sort /@ neighbourhoodEquivalences];
    templateEquivalences = TemplateVarFromNeighbourhood /@ # & /@ neighbourhoodEquivalences;
    transformedEquivalences = {#[[2]], transform[#[[1]]]} & /@ templateEquivalences;
    equivalenceSubsets = Subsets[transformedEquivalences, {(symmetryValue - invariantNeighborhoodQuantity)/2}, templateQuantity];
    unequalComplements = Complement[transformedEquivalences, #] & /@ equivalenceSubsets;
    equations = Apply[Equal, #] & /@ # & /@ equivalenceSubsets;
    inequations = (#[[1]] == 1 - #[[2]]) & /@ # & /@ unequalComplements;
    systems = MapThread[Join[#1, #2] &, {equations, inequations}];
    systems = systems /. x__ == y__ -> Rule[x, y] ;
    templateLists = BaseTemplateCore[k, r] /. systems;
    BuildTemplate[k, r, #] & /@ templateLists
  ];


End[];
EndPackage[];
