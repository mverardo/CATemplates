(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`ExceptionTemplates`",
  {
    "CATemplates`Basic`"
  }
];

PossibleInvalidSubsets::usage = "bl1";
SubstitutionRangeVar::usage = "bl1";
ExpandVar::usage = "bl1";
ExpandVars::usage = "bl1";
InvalidSubSets::usage = "bl1";
ExceptionTemplates::usage= "ExceptionTemplates[t_List, k_Integer:2, r_Integer:2] generate all the templates with variable assignments that make the template t with k colors and r range invalid.";

Begin["`Private`"];
PossibleInvalidSubsets[intemplate_Association] := PossibleInvalidSubsets[intemplate[["rawList"]]];
PossibleInvalidSubsets[intemplate_List] := Select[intemplate, (Depth[#] > 1) &];

SubstitutionRangeVar[var_, k_Integer : 2] := Range[0, (k^Length[RuleTemplateVars[var]]) - 1];

ExpandVar[var_, i_Integer, k_Integer : 2] := Module[{variables = RuleTemplateVars[var], substitution, transformationRules, result},
  substitution = Reverse[IntegerDigits[i, k, Length[variables]]];
  transformationRules = MapThread[#1 -> #2 &, {variables, substitution}];
  result = var /. transformationRules;
  If[result >= k|| result < 0, transformationRules, (##) &[]]
];
ExpandVars[var_, range_, k_Integer : 2] := Module[{result}, result = ExpandVar[var, #, k]& /@ range; If[result == {}, (##) &[], result]];

InvalidSubSets[intemplate_List, k_Integer : 2] := Module[{possibleInvalids = PossibleInvalidSubsets[intemplate], result},
  result = MapThread[ExpandVars, {possibleInvalids, SubstitutionRangeVar[#, k]& /@ possibleInvalids}];
  If[result == {}, {}, Flatten[result,1]]
];

InvalidSubSets[intemplate_Association] := Module[{possibleInvalids = PossibleInvalidSubsets[intemplate]},
  MapThread[ExpandVars, {possibleInvalids, SubstitutionRangeVar[#, intemplate[["k"]]]& /@ possibleInvalids}]
];

ExceptionTemplates[intemplate_] :=
    ExceptionTemplates[intemplate, 2, 1];

ExceptionTemplates[intemplate_, k_ /; k === 2, r_Integer : 1] :=
    Module[{invalidSubsSets, filteredInvalidSubsSets, result = {}},
      invalidSubsSets = Union[
        SortBy[InvalidSubSets[intemplate, k], Total]
      ];
      If[invalidSubsSets =!= {},
        filteredInvalidSubsSets = MinimizedRuleSets[invalidSubsSets];
        result = (OldBaseTemplate[k, r] /. #) & /@ filteredInvalidSubsSets;
      ];
      result
    ];

ExceptionTemplates[intemplate_, k_Integer:2, r_Integer:1] :=
    MapThread[If[#2 === _, #1, #2]&, {OldBaseTemplate[k, r], #}]& /@ Union[(If[NumberQ[#], #, _]& /@ #)& /@ ((OldBaseTemplate[k, r] /. #[[1]])& /@ Cases[{#[[2]], #[[1]] /. #[[2]]}& /@ Flatten[Outer[List, {#[[1]]}, #[[2]], 1]& /@ ({#[[2]], MapThread[#1 -> #2&, {#[[1]], #[[2]]}]& /@ #[[1]]}& /@ ({First@Outer[List, {#[[1]]}, #[[2]], 1], #[[3]]}& /@ ({#[[1]], Tuples[Range[0, k - 1], Length[#[[1]]]], #[[2]]}& /@ ({RuleTemplateVars[{#}], #}& /@ Select[intemplate, (Depth[#] > 1)&])))), 2], {_, x_ /; \[Not]MemberQ[Range[0, k - 1], x]}])]

End[];
EndPackage[];