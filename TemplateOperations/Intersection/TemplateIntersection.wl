(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`",
    "CATemplates`TemplateOperations`Intersection`Common`",
    "CATemplates`TemplateOperations`Intersection`RawIntersection`",
    "CATemplates`TemplateOperations`Intersection`RestrictedTemplateIntersection`"
  }];

TemplateIntersection::usage = "TemplateIntersection[template1_Association, template2_Association]: Receives two templates template1 and template2, and finds a third template that represents their intersection.";

Begin["`Private`"];

TemplateIntersection[template1_Association, template2_Association] :=
    With[{
      k = k[template1],
      r = r[template1],
      expansion = postExpansionFn[template1],
      rawTemplate1 = RawTemplate[kAryRuleTemplate[template1]],
      rawTemplate2 = RawTemplate[kAryRuleTemplate[template2]],
      imprisonmentExpressions = Join[ImprisonmentExpressions[kAryRuleTemplate[template1]], ImprisonmentExpressions[kAryRuleTemplate[template2]]]},
      If[imprisonmentExpressions === {},
        BuildTemplate[k, r, RawIntersection[rawTemplate1, rawTemplate2], expansion],
        BuildTemplate[k, r, RestrictedTemplateIntersection[rawTemplate1, rawTemplate2, imprisonmentExpressions], expansion]]];

(* The intersection between two sets of templates is given by the outer product of the intersection over the sets. *)
TemplateIntersection[x_List, y_List] :=
    Select[Flatten[TemplateIntersection[#[[1]], #[[2]]] & /@ Flatten[Outer[{#1, #2} &, x, y, 1], 1], 1], (# != {} && ValidTemplateQ[#]) &];

End[];
EndPackage[];
