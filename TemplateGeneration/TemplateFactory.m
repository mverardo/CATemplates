(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`TemplateFactory`", "CATemplates`TemplateOperations`Expansion`RawExpansion`"];


BuildTemplate::usage=
"BuildTemplate[k_Integer, r_Real, rawList_List, expansion_Function]
  Builds a template that represents the subspace of the CA space given by k and r, described by the variables in <rawList>.
  <expansion> is the function used by ExpandTemplate to expand the built template.
BuildTemplate[k_Integer, r_Real, rawList_List, expansion_Function, N_Integer]
  <N> is the value of modulus used by the template generator to create <rawList>.";


BaseTemplate::usage="BaseTemplate[k_Integer, r_Real] := Gives the base template for the space of radius r k-ary rules.";


Begin["`Private`"];


BuildTemplate[k_Integer, r_Real, rawList_List] :=
    BuildTemplate[k, r, rawList, RawExpansion];


BuildTemplate[k_Integer, r_Real, rawList_List, expansion_] :=
    Association["k" -> k, "r" -> r, "rawList" -> rawList, "expansionFunction" -> expansion];


BuildTemplate[k_Integer, r_Real, rawList_List, expansion_, N_Integer] :=
    Association["k" -> k, "r" -> r, "rawList" -> rawList, "expansionFunction" -> expansion, "N" -> N];


BaseTemplate[k_Integer, r_Real] :=
    With[{list = Symbol["x" <> ToString[#]] & /@ Range[(k^(Ceiling[r * 2] + 1)) -1, 0, -1]},
      BuildTemplate[k, r, list, RawExpansion]
    ];


End[];
EndPackage[];
