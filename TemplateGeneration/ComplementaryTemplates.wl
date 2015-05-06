(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`ComplementaryTemplates`","CATemplates`Basic`"];


ComplementaryTemplates::usage= "ComplementaryTemplates[t_List, r_Integer : 1]: Generates a set of templates that represent the complementary rules of the template t with radius r. Currently, only works for binary templates.";


Begin["`Private`"];


ComplementaryTemplates[t_List, r_Integer:1]:=
  With[{
      complementEquation = Apply[Or,#&/@Map[Part[#,1]==1-(Part[#,2])&,Select[MapThread[If[#1===#2,"X",#1==#2]&,{BaseTemplate[2,r],t}],Not[#==="X"]&]]],
      baseTemplate = BaseTemplate[2,r]
    },
    Join[If[Solve[complementEquation]==={},{},baseTemplate/.Solve[complementEquation]],MapThread[If[#2=== _,#1,#2]&,{baseTemplate,#}]&/@ExceptionTemplates[2, r, t]]
  ];


End[];
EndPackage[];
