(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
  {
    "CATemplates`Basic`", 
    "CATemplates`TemplateOperations`Intersection`Common`",
    "CATemplates`TemplateOperations`Intersection`RawIntersection`"
  }];


TemplateIntersection::usage= "TemplateIntersection[t1_List, t2_List]: Receives two templates t1 and t2, and finds a third template that represents their intersection.";


Begin["`Private`"];


VarAssignmentsToImprisonmentExpressions::usage="ToImprisonmentExpression[varAssignments_List]: Receives a list of assignments for a template's variables and returns the equivalent ImprisonmentExpressions."
VarAssignmentsToImprisonmentExpressions[varAssignments_List] :=
  #[[1,1]] \[Element] Union[Last /@ #] & /@ Transpose[varAssignments];


ImprisonmentExpressionsToReplacementRules::usage = "ImprisonmentExpressionToReplacementRules[imprisonmentExpressions_List]: Takes a list of imprisonment expressions, and returns a list of replacement rules to be applied to a template."
ImprisonmentExpressionsToReplacementRules[imprisonmentExpressions_List] :=
  If[Length[#[[2]]] == 1, #[[1]] -> #[[2,1]],#[[1]] -> #]& /@ imprisonmentExpressions;


RestrictedTemplateIntersection::usage="RestrictedTemplateIntersection[rawTemplate1_List, rawTemplate2_List, imprisonmentExpressions_List]: Receives rawTemplate1, rawTemplate2, and a list of imprisonment expressions, and returns the intersection of rawTemplate1 and rawTemplate2 with the corresponding value restrictions taken in account."
RestrictedTemplateIntersection[rawTemplate1_List, rawTemplate2_List, imprisonmentExpressions_List]:=
  Module[{rawReplacementRules, valueRestrictions, varAssignments, varReplacementRules},
    rawReplacementRules = First[ReplacementRules[rawTemplate1, rawTemplate2]];
    valueRestrictions = (ValueRestrictions /@ imprisonmentExpressions) /. rawReplacementRules;
    varAssignments = Quiet[Solve[valueRestrictions]];
    If[rawReplacementRules === {} || varAssignments === {},
      Return[{}];
    ];
    varReplacementRules = ImprisonmentExpressionsToReplacementRules[VarAssignmentsToImprisonmentExpressions[varAssignments]];
	  rawTemplate1 /. rawReplacementRules /. varReplacementRules
 ]


TemplateIntersection[template1_, template2_] :=
  With[{
    rawTemplate1 = RawTemplate[template1],
    rawTemplate2 = RawTemplate[template2],
    imprisonmentExpressions = Join[ImprisonmentExpressions[template1], ImprisonmentExpressions[template2]]},
    If[imprisonmentExpressions === {}, 
      RawIntersection[rawTemplate1, rawTemplate2],
      RestrictedTemplateIntersection[rawTemplate1, rawTemplate2, imprisonmentExpressions]]];


(* The intersection between two sets of templates is given by the outer product of the intersection over the sets. *)
TemplateIntersection[x_ /; MatchQ[x, {{__} ..}], y_ /; MatchQ[y, {{__} ..}]] :=
  Select[Flatten[TemplateIntersection[#[[1]], #[[2]]] & /@ Flatten[Outer[{#1, #2} &, x, y, 1], 1], 1], (# != {} && ValidTemplateQ[#]) &];


End[];
EndPackage[];
