(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`",
    "CATemplates`TemplateOperations`Intersection`Common`"
  }];

RawIntersection::usage= "RawIntersection[t1_List, t2_List]: Receives two templates t1 and t2, and finds a third template that represents their intersection. Both arguments should be raw templates, i.e. templates that don't have special sintax constructs.";

ModularTemplateIntersection::usage = "TemplateIntersection[template1_Association, template2_Association]: Receives two modular templates template1 and template2, and finds a third template that represents their intersection.";

TemplateIntersection::usage = "TemplateIntersection[template1_Association, template2_Association]: Receives two templates template1 and template2, and finds a third template that represents their intersection.";

RestrictedTemplateIntersection::usage="RestrictedTemplateIntersection[rawTemplate1_List, rawTemplate2_List, imprisonmentExpressions_List]: Receives rawTemplate1, rawTemplate2, and a list of imprisonment expressions, and returns the intersection of rawTemplate1 and rawTemplate2 with the corresponding value restrictions taken in account.";

Begin["`Private`"];

VarAssignmentsToImprisonmentExpressions::usage="ToImprisonmentExpression[varAssignments_List]: Receives a list of assignments for a template's variables and returns the equivalent ImprisonmentExpressions."
VarAssignmentsToImprisonmentExpressions[varAssignments_List] :=
    #[[1,1]] \[Element] Union[Last /@ #] & /@ Transpose[varAssignments];

ImprisonmentExpressionsToReplacementRules::usage = "ImprisonmentExpressionToReplacementRules[imprisonmentExpressions_List]: Takes a list of imprisonment expressions, and returns a list of replacement rules to be applied to a template."
ImprisonmentExpressionsToReplacementRules[imprisonmentExpressions_List] :=
    If[Length[#[[2]]] == 1, #[[1]] -> #[[2,1]],#[[1]] -> #]& /@ imprisonmentExpressions;

ValueRestrictionIntersection[{}, valueRestrictions_, replacementRules_] := {};

ValueRestrictionIntersection[currentIntersectionResult_, {}, replacementRules_] := currentIntersectionResult;

ValueRestrictionIntersection[currentIntersectionResult_, valueRestrictions_, replacementRules_] :=
    With[{
      varAssignments = Quiet[Solve[First[(ValueRestrictions /@ valueRestrictions) /. replacementRules]]]},
      (* If varAssignments === {}, variable restrictions can't be satisfied on both templates (the system has no solution). Thus, no intersection. *)
      If[varAssignments === {},
        {},
        currentIntersectionResult /.
            ImprisonmentExpressionsToReplacementRules[VarAssignmentsToImprisonmentExpressions[varAssignments]]]];

ModTemplateQ[template_Association] := False;

(*TODO: Move this logic to ReplacementRules function*)
ReplacementRulesIntern[template1_Association, template2_Association] :=
    With[{
      k = k[template1],
      rawTemplate1 = RawTemplate[kAryRuleTemplate[template1]],
      rawTemplate2 = RawTemplate[kAryRuleTemplate[template2]]},
      If[ModTemplateQ[template1] || ModTemplateQ[template2],
        ReplacementRules[rawTemplate1, rawTemplate2, k],
        ReplacementRules[rawTemplate1, rawTemplate2]]];

SimpleIntersection[replacementRules_, template1_Association, template2_Association] :=
    With[{
      coreTemplate1 = RawTemplate[kAryRuleTemplate[template1]],
      coreTemplate2 = RawTemplate[kAryRuleTemplate[template2]]},
      If[replacementRules == {},
        {},
        First[Union[coreTemplate1 /.replacementRules, coreTemplate2 /. replacementRules]]]]


ModIntersection[replacementRules_, template1_Association, template2_Association] :=
    With[{
      coreTemplate1 = RawTemplate[kAryRuleTemplate[template1]]},
      If[replacementRules == {},
        {},
        (*When a modular template returns 2 different sets of replacement rules, they both have equivalent expansions.
          For that reason, only the first set is taken into account. *)
        coreTemplate1 /. ConstantsToVariables[First[replacementRules]]]];


IntersectionFn[template1_Association, template2_Association] :=
    If[ModTemplateQ[template1] || ModTemplateQ[template2],
      ModIntersection,
      SimpleIntersection];

TemplateIntersection[template1_Association, template2_Association] :=
    Module[{
      k = k[template1],
      r = r[template1],
      expansion = postExpansionFn[template1],
      replacementRules = ReplacementRulesIntern[template1, template2],
      intersectionFn = IntersectionFn[template1, template2],
      valueRestrictions = Join[ImprisonmentExpressions[kAryRuleTemplate[template1]], ImprisonmentExpressions[kAryRuleTemplate[template2]]],
      intersectionResult},

      intersectionResult = ValueRestrictionIntersection[intersectionFn[replacementRules, template1, template2], valueRestrictions, replacementRules];
      If[intersectionResult === {},
        EmptyTemplate[],
        BuildTemplate[k, r, intersectionResult, expansion]]];

(* The intersection between two sets of templates is given by the outer product of the intersection over the sets. *)
TemplateIntersection[x_List, y_List] :=
    Select[Flatten[TemplateIntersection[#[[1]], #[[2]]] & /@ Flatten[Outer[{#1, #2} &, x, y, 1], 1], 1], (# != {} && ValidTemplateQ[#]) &];

End[];
EndPackage[];
