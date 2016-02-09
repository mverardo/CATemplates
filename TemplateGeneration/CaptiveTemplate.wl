(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`CaptiveTemplate`", "CATemplates`Basic`"]


CaptiveTemplate::usage="Generates a template representative of all the captive rules of a given space (defined by k and r)."


Begin["`Private`"];

CaptiveNeighborhood[nb_List, k_Integer] :=
    With[{nbRange = Union[nb]},
      Which[
        Length[nbRange] == 1       , First[nbRange],
        nbRange === Range[0, k - 1], TemplateVarFromNeighbourhood[nb, k],
        True                       , TemplateVarFromNeighbourhood[nb, k] \[Element] nbRange]];


CaptiveTemplate[k_Integer: 2, r_Integer: 1] :=
 CaptiveNeighborhood[#, k] & /@ AllNeighbourhoods[k, r];


End[];
EndPackage[];
