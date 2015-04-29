(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`ComplementTemplates`","CATemplates`Basic`"];


ComplementTemplates::usage= "ComplementTemplates[\!\(\*
StyleBox[
StyleBox[\"r\", \"TI\"],\nFontSlant->\"Italic\"]\), \*
StyleBox[\(\!\(\*
StyleBox[
StyleBox[\"t\", \"TI\"],\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"emplate\", \"TI\"]\)\)]] generate a set of templates that represent the completary rules of the \*
StyleBox[\(\!\(\*
StyleBox[
StyleBox[\"t\", \"TI\"],\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"emplate\", \"TI\"]\)\)]\!\(\*
StyleBox[\"  \", \"TI\"]\)with the \!\(\*
StyleBox[
StyleBox[\"r\", \"TI\"],\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \", \"TI\"]\)radius.
ComplementTemplates[\*
StyleBox[\(\!\(\*
StyleBox[\"t\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"emplate\", \"TI\"]\)\)]] generate a set of templates that represent the completary rules of the \*
StyleBox[\(\!\(\*
StyleBox[
StyleBox[\"t\", \"TI\"],\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\"emplate\", \"TI\"]\)\)]\!\(\*
StyleBox[\"  \", \"TI\"]\)with the radius 1\!\(\*
StyleBox[\";\", \"TI\"]\)";


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
