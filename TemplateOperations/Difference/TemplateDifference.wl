(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`TemplateDifference`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`IdentityFn`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`FilterNotAllowed`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`FilterOutOfRange`",
    "CATemplates`TemplateOperations`Difference`Common`",
    "CATemplates`TemplateOperations`Difference`RawDifference`",
    "CATemplates`TemplateOperations`Difference`ExceptionTemplates`"}
];


TemplateDifference::usage = "bl1";
TemplateDifferenceDirect::usage = "bl1";
ExpansionFunctionSelected::usage = "bl1";

Begin["`Private`"];

ExpansionFunctionSelected[x_, y_] := Switch[{x, y},
  (*{IdentityFn, IdentityFn}, FilterOutOfRange,*)
  {IdentityFn, FilterOutOfRange}, FilterOutOfRange,
  _, x
];

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
      expansion = ExpansionFunctionSelected[template1[["postExpansionFn"]], template2[["postExpansionFn"]]],
      rawTemplate1 = RawTemplate[template1[["core"]]],
      rawTemplate2 = RawTemplate[template2[["core"]]],
      imprisonmentExpressions = Join[ImprisonmentExpressions[template1[["core"]]], ImprisonmentExpressions[template2[["core"]]]]
    },
      If[imprisonmentExpressions =!= {},
        TemplateDifference::imprisonmentExpressions = "The result doesn't consider the 'imprisonmentExpressions' because 'TemplateDifference' function still doesn't support that";
        Message[TemplateDifference::imprisonmentExpressions];
      ];
      If[MissingQ[n],
        result = BuildTemplate[k, r, #, expansion]& /@ RawDifference[template1, template2, r];,
        result = BuildTemplate[k, r, #, expansion, n]& /@ RawDifference[template1, template2, r];
      ];
      result
    ];

TemplateDifferenceDirect[template1_Association, template2_Association] :=
    With[{
      k = template1[["k"]],
      r = template1[["r"]],
      n = template1[["N"]],
      expansion = ExpansionFunctionSelected[template1[["postExpansionFn"]], template2[["postExpansionFn"]]],
      rawTemplate1 = RawTemplate[template1[["core"]]],
      rawTemplate2 = RawTemplate[template2[["core"]]],
      imprisonmentExpressions = Join[ImprisonmentExpressions[template1[["core"]]], ImprisonmentExpressions[template2[["core"]]]]
    },
      If[imprisonmentExpressions =!= {},
        TemplateDifference::imprisonmentExpressions = "The result doesn't consider the 'imprisonmentExpressions' because 'TemplateDifference' function still doesn't support that";
        Message[TemplateDifference::imprisonmentExpressions];
      ];
      If[MissingQ[n],
        result = BuildTemplate[k, r, #, expansion]& /@ RawDifferenceDirect[template1, template2, r];,
        result = BuildTemplate[k, r, #, expansion, n]& /@ RawDifferenceDirect[template1, template2, r];
      ];
      result
    ];

End[];
EndPackage[];