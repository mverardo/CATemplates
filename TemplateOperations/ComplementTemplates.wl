(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`TemplateIntersection`", "CATemplates`Basic`"];


ComplementTemplates::usage= "Put documentation here";


Begin["`Private`"];


GetEquation[t_]:=Select[MapThread[If[#1===#2,"X",#1==#2]&,{BaseTemplate[],t}],Not[#==="X"]&];

GetEquationComplements[t_]:=Apply[Or,#&/@Map[Part[#,1]==1-(Part[#,2])&,GetEquation[t]]];

ComplementTemplates[k_Integer:2,r_:1,t_]:=Join[BaseTemplate[] /.Solve[GetEquationComplements[t]],MapThread[If[#2=== _,#1,#2]&,{BaseTemplate[],#}]&/@ExceptionTemplates[k, r, t]];


End[];
EndPackage[];
