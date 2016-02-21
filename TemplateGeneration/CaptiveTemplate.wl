(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`CaptiveTemplate`", {"CATemplates`Basic`", "CATemplates`TemplateGeneration`TemplateFactory`", "CATemplates`TemplateOperations`Expansion`RestrictedExpansion`"}]


CaptiveTemplate::usage="Generates a template representative of all the captive rules of a given space (defined by k and r)."


Begin["`Private`"];

CaptiveNeighborhood[nb_List, k_Integer] :=
    With[{nbRange = Union[nb]},
      Which[
        Length[nbRange] == 1       , First[nbRange],
        nbRange === Range[0, k - 1], TemplateVarFromNeighbourhood[nb, k],
        True                       , TemplateVarFromNeighbourhood[nb, k] \[Element] nbRange]];


CaptiveTemplate[k_Integer: 2, r_Real: 1.0] :=
 BuildTemplate[k, r, CaptiveNeighborhood[#, k] & /@ AllNeighbourhoods[k, r], RestrictedExpansion];


End[];
EndPackage[];
