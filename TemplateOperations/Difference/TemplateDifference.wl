(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`TemplateDifference`",
  {
    "CATemplates`Basic`",
    "CATemplates`TemplateGeneration`TemplateFactory`",
    "CATemplates`TemplateOperations`Difference`Common`",
    "CATemplates`TemplateOperations`Intersection`Common`",
    "CATemplates`TemplateOperations`Intersection`RawIntersection`"}
];


TemplateDifference::usage = "bl1";
DualEquationSystem::usage = "bl2";
DifferenceReplacementRules::usage = "bl3";

Begin["`Private`"];

TemplateDifference[template1_List, template2_List, radius_ : 1] :=
    Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      templateIntersection = RawIntersection[template1, template2]//Flatten;
      If[!ValidTemplateQ[templateIntersection] || templateIntersection === {},
        template1,
        replacementRules = DifferenceReplacementRules[template1, templateIntersection];
        replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];(*ATENÇÃO: Aqui remove todas as regras que contenham números racionais*)


        If[replacementRulesFinal == {}, {},
          templateDifferenceP1 = template1 /. replacementRulesFinal;(*Apply*)
          exceptionTemplates = ExceptionTemplates[templateIntersection, 2, radius];
          templateDifferenceP2 = RawIntersection[template1, #] & /@ exceptionTemplates;
          templateDifference = Join[templateDifferenceP1, templateDifferenceP2]
        (*cleanTemplateDifference = DeleteCases[Union[templateDifference], template1]*)
        ]
      ]
    ];



End[];
EndPackage[];
