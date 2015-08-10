(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`SymmetricTemplate`", {"CATemplates`Basic`", "CATemplates`TemplateGeneration`TemplateFactory`", "CATemplates`TemplateOperations`Expansion`RawExpansion`"}];


TotalisticTemplate::usage="Returns a template that represents all of the totalistic rules in a given space (defined by k and r).";


OuterTotalisticTemplate::usage="Returns a template that represents all of the outer totalistic rules in a given space (defined by k and r).";


Begin["`Private`"];

AggregatedTemplateList::usage = "AggregatedTemplate[k_Integer: 2, r_Real: 1, aggregatorFunction_ ] := Generates a template list in which every neighbourhood that shares a given metric has the same variable.
The aggregator function receives a neighbourhood and should return a list with the neighbourhood in its first position, and the metric value in the second. For totalistic rules: {#, Plus @@ #} &";

AggregatedTemplateList[k_Integer: 2, r_Real: 1, aggregatorFunction_ ] :=
    Module[{aggregatedNeighborhoods, symbolSubscripts, lastNeighborhoodsWithValue},
      aggregatedNeighborhoods = aggregatorFunction /@ AllNeighbourhoods[k, r];
      lastNeighborhoodsWithValue = Last[Cases[aggregatedNeighborhoods, {__, #[[2]]}]] & /@ aggregatedNeighborhoods;
      symbolSubscripts = FromDigits[#[[1]], k] & /@ lastNeighborhoodsWithValue;
      Symbol["x" <> ToString[#]] & /@ symbolSubscripts];


TotalisticTemplate[k_Integer: 2, r_Real: 1] :=
    With[
      {templateList = AggregatedTemplateList[k, r, {#, Plus @@ #} &]},
      BuildTemplate[k, r, templateList, RawExpansion]
    ];


OuterTotalisticTemplate[k_Integer: 2, r_Real: 1] :=
    With[
      {templateList = AggregatedTemplateList[k, r, {#, FromDigits[ToString[(Plus @@ #) - #[[Ceiling[Length[#]/2]]]] <> ToString[#[[Ceiling[Length[#]/2]]]]]} &]},
      BuildTemplate[k, r, templateList, RawExpansion]
    ];


End[];
EndPackage[];
