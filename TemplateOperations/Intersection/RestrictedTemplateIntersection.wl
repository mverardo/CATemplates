BeginPackage["CATemplates`TemplateOperations`Intersection`RestrictedTemplateIntersection`", {"CATemplates`Basic`", "CATemplates`TemplateOperations`Intersection`Common`"}];

RestrictedTemplateIntersection::usage="RestrictedTemplateIntersection[rawTemplate1_List, rawTemplate2_List, imprisonmentExpressions_List]: Receives rawTemplate1, rawTemplate2, and a list of imprisonment expressions, and returns the intersection of rawTemplate1 and rawTemplate2 with the corresponding value restrictions taken in account.";

Begin["`Private`"];

VarAssignmentsToImprisonmentExpressions::usage="ToImprisonmentExpression[varAssignments_List]: Receives a list of assignments for a template's variables and returns the equivalent ImprisonmentExpressions."
VarAssignmentsToImprisonmentExpressions[varAssignments_List] :=
    #[[1,1]] \[Element] Union[Last /@ #] & /@ Transpose[varAssignments];


ImprisonmentExpressionsToReplacementRules::usage = "ImprisonmentExpressionToReplacementRules[imprisonmentExpressions_List]: Takes a list of imprisonment expressions, and returns a list of replacement rules to be applied to a template."
ImprisonmentExpressionsToReplacementRules[imprisonmentExpressions_List] :=
    If[Length[#[[2]]] == 1, #[[1]] -> #[[2,1]],#[[1]] -> #]& /@ imprisonmentExpressions;


RestrictedTemplateIntersection[rawTemplate1_List, rawTemplate2_List, imprisonmentExpressions_List]:=
    Module[{rawReplacementRules, firstSolution, valueRestrictions, varAssignments, varReplacementRules},
      rawReplacementRules = ReplacementRules[rawTemplate1, rawTemplate2];
      (* If rawReplacementRules === {}, the system obtained by equating rawTemplate1 and rawTemplate2 has no solution. Thus there is no intersection.*)
      If[rawReplacementRules === {},
        Return[{}];
      ];
      firstSolution = First[rawReplacementRules];
      valueRestrictions = (ValueRestrictions /@ imprisonmentExpressions) /. firstSolution;
      varAssignments = Quiet[Solve[valueRestrictions]];
      (* If varAssignments === {}, variable restrictions can't be satisfied on both templates (the system has no solution). Thus, no intersection. *)
      If[varAssignments === {},
        Return[{}];
      ];
      varReplacementRules = ImprisonmentExpressionsToReplacementRules[VarAssignmentsToImprisonmentExpressions[varAssignments]];
      rawTemplate1 /. firstSolution /. varReplacementRules
    ];

End[]; (* `Private` *)

EndPackage[];
