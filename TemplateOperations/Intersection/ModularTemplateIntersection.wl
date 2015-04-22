(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`ModularTemplateIntersection`", 
  { 
    "CATemplates`Basic`", 
    "CATemplates`TemplateOperations`Intersection`Common`"}];


ModularTemplateIntersection::usage = "TemplateIntersection[t1_List, t2_List]: Receives two modular templates t1 and t2, and finds a third template that represents their intersection.";


Begin["`Private`"];


ModularTemplateIntersection[template1_, template2_, k_] :=
  Module[{replacementRules, convertedReplacementRules},
	replacementRules = ReplacementRules[template1, template2, k];
    If[replacementRules == {}, 
      Return[{}]
    ];
    (*When a modular template returns 2 different sets of replacement rules, they are both equivalents in terms of ExpandTemplateModK. 
      For that reason, only one of the returned sets is taken into account. *)
    convertedReplacementRules = ConstantsToVariables[First[replacementRules]];
    template1 /. convertedReplacementRules];


End[];
EndPackage[];
