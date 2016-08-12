(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`RawDifference`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`",
    "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
    "CATemplates`TemplateOperations`Difference`Common`",
    "CATemplates`TemplateOperations`Difference`ExceptionTemplates`"}
];


RawDifference::usage = "bl1";
RawDifferenceOld::usage = "bl1";
RawDifferenceDirect::usage = "bl1";
RawDifferenceDirectOld::usage = "bl1";

Begin["`Private`"];

RawDifferenceOld[template1_List, template2_List, radius_ : 1.0] :=
    Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      templateIntersection = Flatten[RawIntersection[template1, template2]];
      If[!ValidTemplateCoreQ[templateIntersection] || templateIntersection === {},
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

RawDifferenceDirectOld[template1_List, template2_List, radius_ : 1.0] :=
    Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      templateIntersection = Flatten[RawIntersection[template1, template2]];
      If[!ValidTemplateCoreQ[templateIntersection] || templateIntersection === {},
        templateDifference = {template1};,
        replacementRules = DifferenceReplacementRules[template1, template2];
        replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];
        replacementRulesFinal = Select[replacementRules, ContainsOnly[TemplateCoreVars[#], TemplateCoreVars[template1]]&];

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

RawDifference[template1A_Association, template2A_Association, radius_ : 1.0] :=
    Module[{template1, template2, templateIntersection, template1And2HasIntersection, templateIntersectionA, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      template1 = template1A[["core"]];
      template2 = template2A[["core"]];
      templateIntersectionA = TemplateIntersection[template1A,template2A];
      templateIntersection = templateIntersectionA[["core"]];
      template1And2HasIntersection = !(!ValidTemplateCoreQ[templateIntersection] || templateIntersection === {});
      If[!template1And2HasIntersection,
        templateDifference = {template1};,
        replacementRules = DifferenceReplacementRules[template1, templateIntersection];
        replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];
        replacementRulesFinal = Select[replacementRules, ContainsOnly[TemplateCoreVars[#], TemplateCoreVars[template1]]&];

        If[replacementRulesFinal == {},
          templateDifferenceP1 = {};,
          templateDifferenceP1 = template1 /. replacementRulesFinal;
        ];
        exceptionTemplates = ExceptionTemplates[templateIntersectionA];
        templateDifferenceP2 = TemplateIntersection[template1A, #] & /@ exceptionTemplates;
        templateDifferenceP2 = #[["core"]] & /@templateDifferenceP2;
        templateDifference = Join[templateDifferenceP1, templateDifferenceP2];
      ];
      templateDifference
    ];


RawDifferenceDirect[template1A_Association, template2A_Association, radius_ : 1.0] :=
    Module[{template1, template2, templateIntersection, template1And2HasIntersection, templateIntersectionA, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
      template1 = template1A[["core"]];
      template2 = template2A[["core"]];
      templateIntersectionA = TemplateIntersection[template1A,template2A];
      templateIntersection = templateIntersectionA[["core"]];
      template1And2HasIntersection = !(!ValidTemplateCoreQ[templateIntersection] || templateIntersection === {});
      If[!template1And2HasIntersection,
        templateDifference = {template1};,
        replacementRules = DifferenceReplacementRules[template1, template2];
        replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];
        replacementRulesFinal = Select[replacementRules, ContainsOnly[TemplateCoreVars[#], TemplateCoreVars[template1]]&];

        If[replacementRulesFinal == {},
          templateDifferenceP1 = {};,
          templateDifferenceP1 = template1 /. replacementRulesFinal;
        ];
        exceptionTemplates = ExceptionTemplates[template2A];
        templateDifferenceP2 = TemplateIntersection[template1A, #] & /@ exceptionTemplates;
        templateDifferenceP2 = #[["core"]] & /@templateDifferenceP2;
        templateDifference = Join[templateDifferenceP1, templateDifferenceP2];
      ];
      templateDifference
    ];



End[];
EndPackage[];
