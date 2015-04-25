(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`TemplateIntersection`", "CATemplates`Basic`"];


ExceptionTemplates::usage= "Put documentation here";


Begin["`Private`"];


ExceptionPatterns[k_Integer:2,r_:1,intemplate_]:=
MapThread[If[#2=== _,#1,#2]&,{BaseTemplate[],#}]&/@Union[(If[NumberQ[#],#,_]&/@#)&/@((BaseTemplate[k,r]/.#[[1]])&/@Cases[{#[[2]],#[[1]]/.#[[2]]}&/@Flatten[Outer[List,{#[[1]]},#[[2]],1]&/@({#[[2]],MapThread[#1->#2&,{#[[1]],#[[2]]}]&/@#[[1]]}&/@({First@Outer[List,{#[[1]]},#[[2]],1],#[[3]]}&/@({#[[1]],Tuples[Range[0,k-1],Length[#[[1]]]],#[[2]]}&/@({RuleTemplateVars[{#}],#}&/@Select[intemplate,(Depth[#]>1)&])))),2],{_,x_/;\[Not]MemberQ[Range[0,k-1],x]}])];

ExceptionTemplates[k_Integer:2,r_:1,intemplate_]:=ExceptionPatterns[k,r,intemplate];


End[];
EndPackage[];
