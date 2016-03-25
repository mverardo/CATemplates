(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`ModularTemplateIntersection`",
  {
    "CATemplates`Basic`",
    "CATemplates`TemplateGeneration`TemplateFactory`",
    "CATemplates`TemplateOperations`Intersection`Common`"}];


ModularTemplateIntersection::usage = "TemplateIntersection[template1_Association, template2_Association]: Receives two modular templates template1 and template2, and finds a third template that represents their intersection.";


Begin["`Private`"];

ModularIntersection[template1_, template2_, k_] :=
    Module[{replacementRules, convertedReplacementRules},
      replacementRules = ReplacementRules[template1, template2, k];
      If[replacementRules == {},
        Return[{}]
      ];
      (*When a modular template returns 2 different sets of replacement rules, they both have equivalent expansions.
      For that reason, only one of the returned sets is taken into account. *)
      convertedReplacementRules = ConstantsToVariables[First[replacementRules]];
      template1 /. convertedReplacementRules];

ModularTemplateIntersection[template1_Association, template2_Association] :=
    With[{
      k = template1[["k"]],
      r = template1[["r"]],
      expansion = template1[["expansionFunction"]],
      rawTemplate1 = template1[["rawList"]],
      rawTemplate2 = template2[["rawList"]]},
      BuildTemplate[k, r, ModularIntersection[rawTemplate1, rawTemplate2, k], expansion]];


End[];
EndPackage[];
