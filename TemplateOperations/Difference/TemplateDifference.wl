(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`TemplateDifference`",
  {
    "CATemplates`Basic`",
    "CATemplates`TemplateGeneration`TemplateFactory`",
    "CATemplates`TemplateOperations`Difference`Common`",
    "CATemplates`TemplateOperations`Difference`RawDifference`",
    "CATemplates`TemplateOperations`Difference`ExceptionTemplates`",
    "CATemplates`TemplateOperations`Intersection`RawIntersection`"}
];


TemplateDifference::usage = "bl1";
TemplateDifferenceDirect::usage = "bl1";
ExpansionFunctionSelected::usage = "bl1";

Begin["`Private`"];

ExpansionFunctionSelected[expansion_] := With[{result = expansion},
  If[result === RawExpansion,
    result = FilteredExpansion;
  ];
  result
];

TemplateDifference[template1_Association, template2_Association] :=
    With[{
      k = template1[["k"]],
      r = template1[["r"]],
      n = template1[["N"]],
      expansion = ExpansionFunctionSelected[template1[["expansionFunction"]]],
      rawTemplate1 = RawTemplate[template1[["rawList"]]],
      rawTemplate2 = RawTemplate[template2[["rawList"]]],
      imprisonmentExpressions = Join[ImprisonmentExpressions[template1[["rawList"]]], ImprisonmentExpressions[template2[["rawList"]]]]},
      If[imprisonmentExpressions =!= {},
        TemplateDifference::imprisonmentExpressions = "The result doesn't consider the 'imprisonmentExpressions' because 'TemplateDifference' function still doesn't support that";
        Message[TemplateDifference::imprisonmentExpressions];
      ];
      If[MissingQ[n],
        result = BuildTemplate[k, r, #, expansion]& /@ RawDifference[rawTemplate1, rawTemplate2, r];,
        result = BuildTemplate[k, r, #, expansion, n]& /@ RawDifference[rawTemplate1, rawTemplate2, r];
      ];
      result
    ];

TemplateDifferenceDirect[template1_Association, template2_Association] :=
    With[{
      k = template1[["k"]],
      r = template1[["r"]],
      n = template1[["N"]],
      expansion = ExpansionFunctionSelected[template1[["expansionFunction"]]],
      rawTemplate1 = RawTemplate[template1[["rawList"]]],
      rawTemplate2 = RawTemplate[template2[["rawList"]]],
      imprisonmentExpressions = Join[ImprisonmentExpressions[template1[["rawList"]]], ImprisonmentExpressions[template2[["rawList"]]]]
    },
      If[imprisonmentExpressions =!= {},
        TemplateDifference::imprisonmentExpressions = "The result doesn't consider the 'imprisonmentExpressions' because 'TemplateDifference' function still doesn't support that";
        Message[TemplateDifference::imprisonmentExpressions];
      ];
      If[MissingQ[n],
        result = BuildTemplate[k, r, #, expansion]& /@ RawDifferenceDirect[rawTemplate1, rawTemplate2, r];,
        result = BuildTemplate[k, r, #, expansion, n]& /@ RawDifferenceDirect[rawTemplate1, rawTemplate2, r];
      ];
      result
    ];

End[];
EndPackage[];