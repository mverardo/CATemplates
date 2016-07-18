(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`RawDifference`",
  {
    "CATemplates`Basic`",
    "CATemplates`TemplateGeneration`TemplateFactory`",
    "CATemplates`TemplateOperations`Difference`Common`",
    "CATemplates`TemplateOperations`Difference`ExceptionTemplates`",
    "CATemplates`TemplateOperations`Intersection`RawIntersection`"}
];


RawDifference::usage = "bl1";
RawDifferenceDirect::usage = "bl1";

Begin["`Private`"];

RawDifference[template1_List, template2_List, radius_ : 1] :=
    Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      templateIntersection = Flatten[RawIntersection[template1, template2]];
      If[!ValidTemplateQ[templateIntersection] || templateIntersection === {},
        templateDifference = {template1};,
        replacementRules = DifferenceReplacementRules[template1, templateIntersection];
        replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];

        If[replacementRulesFinal == {}, templateDifference = {};,
          templateDifferenceP1 = template1 /. replacementRulesFinal;
          exceptionTemplates = ExceptionTemplates[templateIntersection, 2, radius];
          templateDifferenceP2 = RawIntersection[template1, #] & /@ exceptionTemplates;
          templateDifference = Join[templateDifferenceP1, templateDifferenceP2];
        ]
      ];
      templateDifference
    ];

RawDifferenceDirect[template1_List, template2_List, radius_ : 1.0] :=
    Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      templateIntersection = Flatten[RawIntersection[template1, template2]];
      If[!ValidTemplateQ[templateIntersection] || templateIntersection === {},
        templateDifference = {template1};,
        replacementRules = DifferenceReplacementRules[template1, template2];
        replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];
        replacementRulesFinal = Select[replacementRules, ContainsOnly[RuleTemplateVars[#], RuleTemplateVars[template1]]&];

        If[replacementRulesFinal == {},
          templateDifferenceP1 = {};,
          templateDifferenceP1 = template1 /. replacementRulesFinal;
        ];
        exceptionTemplates = ExceptionTemplates[template2, 2, radius];
        templateDifferenceP2 = RawIntersection[template1, #] & /@ exceptionTemplates;
        templateDifference = Join[templateDifferenceP1, templateDifferenceP2];
        (*Print[templateDifferenceP1];*)
        (*Print[templateDifferenceP2];*)
      ];
      templateDifference
    ];



End[];
EndPackage[];
