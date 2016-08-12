(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`ExceptionTemplates`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`"
  }
];

PossibleInvalidSubsets::usage = "bl1";
SubstitutionRangeVar::usage = "bl1";
ExpandVar::usage = "bl1";
ExpandVars::usage = "bl1";
InvalidSubSets::usage = "bl1";
ExceptionTemplates::usage= "ExceptionTemplates[t_List, k_Integer:2, r_Real:1] generate all the templates with variable assignments that make the template t with k colors and r range invalid.";

Begin["`Private`"];
PossibleInvalidSubsets[intemplate_Association] := PossibleInvalidSubsets[intemplate[["rawList"]]];
PossibleInvalidSubsets[intemplate_List] := Select[intemplate, (Depth[#] > 1) &];

SubstitutionRangeVar[var_, k_Integer : 2] := Range[0, k^Length[TemplateCoreVars[{var}]] - 1];

ExpandVar[var_, i_Integer] := Module[{variables = TemplateCoreVars[{var}], substitution, transformationRules, result},
  substitution = Reverse[IntegerDigits[i, 2, Length[variables]]];
  transformationRules = MapThread[#1 -> #2 &, {variables, substitution}];
  result = var /. transformationRules;
  If[result >= 2|| result < 0, transformationRules, (##) &[]]
];
ExpandVars[vars_, range_] :=
    Module[{result}, result = Map[ExpandVar[vars, #] &, range];
    If[result == {}, Nothing, result]];

InvalidSubSets[intemplate_List] :=
    Module[{possibleInvalids = PossibleInvalidSubsets[intemplate],
      result},
      result =
          MapThread[
            ExpandVars, {possibleInvalids,
            SubstitutionRangeVar[#, 2] & /@ possibleInvalids}];
      If[result == {}, {}, Flatten[result, 1]]];

InvalidSubSets[intemplate_Association] := Module[{possibleInvalids = PossibleInvalidSubsets[intemplate]},
  MapThread[ExpandVars, {possibleInvalids, SubstitutionRangeVar[#, intemplate[["k"]]]& /@ possibleInvalids}]
];

ExceptionTemplates[intemplate_Association] :=
    Module[{exceptionTemplates},
      exceptionTemplates = ExceptionTemplates[intemplate[["core"]], intemplate[["k"]], intemplate[["r"]]];
      BuildTemplate[intemplate[["k"]], intemplate[["r"]], #] & /@ exceptionTemplates
    ];

ExceptionTemplates[intemplate_List] :=
    ExceptionTemplates[intemplate, 2, 1.0];

ExceptionTemplates[intemplate_List, k_ /; k === 2, r_Real : 1.0] :=
    Module[{invalidSubsSets, filteredInvalidSubsSets, baseTemplate = OldBaseTemplate[k, r], result = {}},
      invalidSubsSets = Union[
        SortBy[InvalidSubSets[intemplate], Total]
      ];
      If[invalidSubsSets =!= {},
        filteredInvalidSubsSets = MinimizedRuleSets[invalidSubsSets];
        result = (baseTemplate /. #) & /@ filteredInvalidSubsSets;
      ];
      result
    ];

ExceptionTemplates[intemplate_List, k_Integer:2, r_Real : 1.0] :=
    MapThread[If[#2 === _, #1, #2]&, {OldBaseTemplate[k, r], #}]& /@ Union[(If[NumberQ[#], #, _]& /@ #)& /@ ((OldBaseTemplate[k, r] /. #[[1]])& /@ Cases[{#[[2]], #[[1]] /. #[[2]]}& /@ Flatten[Outer[List, {#[[1]]}, #[[2]], 1]& /@ ({#[[2]], MapThread[#1 -> #2&, {#[[1]], #[[2]]}]& /@ #[[1]]}& /@ ({First@Outer[List, {#[[1]]}, #[[2]], 1], #[[3]]}& /@ ({#[[1]], Tuples[Range[0, k - 1], Length[#[[1]]]], #[[2]]}& /@ ({TemplateCoreVars[{#}], #}& /@ Select[intemplate, (Depth[#] > 1)&])))), 2], {_, x_ /; \[Not]MemberQ[Range[0, k - 1], x]}])]

End[];
EndPackage[];