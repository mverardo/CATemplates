(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`CaptiveTemplate`", "CATemplates`Basic`"]


CaptiveTemplate::usage="Generates a template representative of all the captive rules of a given space (defined by k and r)."


Begin["`Private`"];


CaptiveTemplate[k_Integer: 2, r_Integer: 1] :=
 With[
	{range = Union[#]},
    If[range === Range[0, k - 1],
		TemplateVarFromNeighbourhood[#, k],
		If[Length[range] == 1,
			First[range],
			TemplateVarFromNeighbourhood[#, k] \[Element] range]]
	] & /@ AllNeighbourhoods[k, r];


End[];
EndPackage[];
