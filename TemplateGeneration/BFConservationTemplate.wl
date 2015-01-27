(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`BFConservationTemplate`", "CATemplates`Basic`"]


BFConservationTemplate::usage="Generates a template representative of all the conservative rules of a given space (defined by k and r)."


Begin["`Private`"];


BFConservationTemplate[k_Integer: 2,r_: 1]:=
Module[{basetemplate,vars,relevantNeighbourhoods,equations},
basetemplate=BaseTemplate[k,r];
relevantNeighbourhoods=Join[{Table[0, {2 r + 1}]}, Cases[AllNeighbourhoods[k, r], {x_ /; x != 0, ___}]];
vars=TemplateVarFromNeighbourhood[#,k]& /@ relevantNeighbourhoods;
equations = (Equal @@ #) & /@ ({RuleOutputFromNeighbourhood[#, 
        basetemplate, k, r],
       First[#] + \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(2  r\)]\((RuleOutputFromNeighbourhood[Join[Table[0, {i}], Take[#, {2, 2  r - i + 2}]], basetemplate, k, r] - RuleOutputFromNeighbourhood[Join[Table[0, {i}], Take[#, {1, 2  r - i + 1}]], basetemplate, k, r])\)\)} & /@ 
     						relevantNeighbourhoods);
First[basetemplate /. Quiet[Solve[equations, vars]]]];


End[];
EndPackage[];
