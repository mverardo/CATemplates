(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`"
  }];

EquationSystem::usage="EquationSystem[t1_List, t2_List]: Receives two templates, t1 and t2, and returns an equation system in which every slot of t1 is equal to the corresponding slot in t2. Ex: EquationSystem[{x1, x0}, {1, x0}] results in {x1 == 1, x0 == x0}.";

ReplacementRules::usage = "
ReplacementRules[t1_List, t2_List]: Takes two templates t1 and t2, and returns the replacement rules that could be applied to t1 or t2 in order to find an intersection of both. Returns {} if there is no possible intersection, or {{}} if they are both templates are the same.
ReplacementRules[t1_List, t2_List, k_Integer]: Takes two templates t1 and t2, and returns the replacement rules that could be applied to t1 or t2 in order to find an intersection of both. Assumes both templates are modular.
";

TemplateIntersection::usage = "TemplateIntersection[template1_Association, template2_Association]: Receives two templates template1 and template2, and finds a third template that represents their intersection.";

Begin["`Private`"];

EquationSystem[template1_List,template2_List]:=
    Equal @@ # & /@ Transpose[{template1, template2}];

ReplacementRules[template1_Association, template2_Association]:=
    Module[{
      k = k[template1],
      rawTemplate1 = RawTemplate[kAryRuleTemplate[template1]],
      rawTemplate2 = RawTemplate[kAryRuleTemplate[template2]],
      templateVars},
      templateVars = SortBy[Union[Flatten[RuleTemplateVars[#] & /@ {rawTemplate1, rawTemplate2}, 1]], FromDigits[StringDrop[SymbolName[#],1]] &];
      If[ModTemplateQ[template1] || ModTemplateQ[template2],
        Quiet[Solve[EquationSystem[rawTemplate1, rawTemplate2], Reverse[templateVars], Modulus -> k]],
        Quiet[Solve[EquationSystem[rawTemplate1, rawTemplate2], templateVars]]]];

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
      replacementRules = ReplacementRules[template1, template2],
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
