(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`ComplementTemplates`","CATemplates`Basic`"];


ComplementTemplates::usage= "Put documentation here";


Begin["`Private`"];


(*GetEquation::usage= "Put documentation here";
GetEquation[r_:1,t_]:=Select[MapThread[If[#1===#2,"X",#1==#2]&,{BaseTemplate[2,r],t}],Not[#==="X"]&];*)


(*GetEquationComplements::usage= "Put documentation here";
GetEquationComplements[r_:1,t_]:=Apply[Or,#&/@Map[Part[#,1]==1-(Part[#,2])&,GetEquation[r,t]]];*)


(*ComplementTemplates[r_:1,t_]:=Join[BaseTemplate[2,r]/.Solve[GetEquationComplements[r,t]],MapThread[If[#2=== _,#1,#2]&,{BaseTemplate[2,r],#}]&/@ExceptionTemplates[2, r, t]];*)


ComplementTemplates[r_:1,t_]:=
With[{
complementEquation = Apply[Or,#&/@Map[Part[#,1]==1-(Part[#,2])&,Select[MapThread[If[#1===#2,"X",#1==#2]&,{BaseTemplate[2,r],t}],Not[#==="X"]&]]],
baseTemplate = BaseTemplate[2,r]
},

Join[baseTemplate/.Solve[complementEquation],MapThread[If[#2=== _,#1,#2]&,{baseTemplate,#}]&/@ExceptionTemplates[2, r, t]]
];


End[];
EndPackage[];
