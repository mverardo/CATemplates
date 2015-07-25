(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`TemplateFactory`", "CATemplates`Basic`"]


BuildTemplate::usage=
"BuildTemplate[rawList_List, k_Integer, r_Real, expansion_Function]
  Builds a template that represents the subspace of the CA space given by k and r, described by the variables in <rawList>.
  <expansion> is the function used by ExpandTemplate to expand the built template.
BuildTemplate[rawList_List, k_Integer, r_Real, expansion_Function, N_Integer]
  <N> is the value of modulus used by the template generator to create <rawList>.";


Begin["`Private`"];


BuildTemplate[k_Integer, r_Real, rawList_List, expansion_] :=
  Association["k" -> k, "r" -> r, "rawList" -> rawList, "expansionFunction" -> expansion];


BuildTemplate[k_Integer, r_Real, rawList_List, expansion_, N_Integer] :=
    Association["k" -> k, "r" -> r, "rawList" -> rawList, "expansionFunction" -> expansion, "N" -> N];


End[];
EndPackage[];
