(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
  {
    "CATemplates`Basic`", 
    "CATemplates`TemplateOperations`Intersection`Common`",
    "CATemplates`TemplateOperations`Intersection`RawIntersection`",
    "CATemplates`TemplateOperations`Intersection`RestrictedTemplateIntersection`"
  }];


TemplateIntersection::usage= "TemplateIntersection[t1_List, t2_List]: Receives two templates t1 and t2, and finds a third template that represents their intersection.";

Begin["`Private`"];

TemplateIntersection[template1_, template2_] :=
  With[{
    rawTemplate1 = RawTemplate[template1],
    rawTemplate2 = RawTemplate[template2],
    imprisonmentExpressions = Join[ImprisonmentExpressions[template1], ImprisonmentExpressions[template2]]},
    If[imprisonmentExpressions === {}, 
      RawIntersection[rawTemplate1, rawTemplate2],
      RestrictedTemplateIntersection[rawTemplate1, rawTemplate2, imprisonmentExpressions]]];


(* The intersection between two sets of templates is given by the outer product of the intersection over the sets. *)
TemplateIntersection[x_ /; MatchQ[x, {{__} ..}], y_ /; MatchQ[y, {{__} ..}]] :=
  Select[Flatten[TemplateIntersection[#[[1]], #[[2]]] & /@ Flatten[Outer[{#1, #2} &, x, y, 1], 1], 1], (# != {} && ValidTemplateQ[#]) &];


End[];
EndPackage[];
