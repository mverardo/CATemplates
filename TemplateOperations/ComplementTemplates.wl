(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`ComplementTemplates`","CATemplates`Basic`"];


ComplementTemplates::usage= "ComplementTemplates[r, t] generate a set of templates that represent the completary rules of the binary template t with the r radius.
ComplementTemplates[t] generate a set of templates that represent the completary rules of the binary template t with the radius 1.";


Begin["`Private`"];


ComplementTemplates[r_:1,t_]:=
With[{
complementEquation = Apply[Or,#&/@Map[Part[#,1]==1-(Part[#,2])&,Select[MapThread[If[#1===#2,"X",#1==#2]&,{BaseTemplate[2,r],t}],Not[#==="X"]&]]],
baseTemplate = BaseTemplate[2,r]
},

Join[baseTemplate/.Solve[complementEquation],MapThread[If[#2=== _,#1,#2]&,{baseTemplate,#}]&/@ExceptionTemplates[2, r, t]]
];


End[];
EndPackage[];
