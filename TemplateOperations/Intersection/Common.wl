(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`Intersection`Common`", "CATemplates`Basic`"];


ConstantsToVariables::usage = "ConstantsToVariables[replacementRules_]: Receives a list of expressions, and converts any symbol of the type C[i_Integer] into its corresponding template variable, preserving the index-variable duality."


EquationSystem::usage="EquationSystem[t1_List, t2_List]: Receives two templates, t1 and t2, and returns an equation system in which every slot of t1 is equal to the corresponding slot in t2. Ex: EquationSystem[{x1, x0}, {1, x0}] results in {x1 == 1, x0 == x0}.";


ReplacementRules::usage = "
ReplacementRules[t1_List, t2_List]: Takes two templates t1 and t2, and returns the replacement rules that could be applied to t1 or t2 in order to find an intersection of both. Returns {} if there is no possible intersection, or {{}} if they are both templates are the same. 
ReplacementRules[t1_List, t2_List, k_Integer]: Takes two templates t1 and t2, and returns the replacement rules that could be applied to t1 or t2 in order to find an intersection of both. Assumes both templates are modular.
";


Begin["`Private`"];


ConstantsToVariables[replacementRules_List] := 
  Module[{freeVariableReplacementRules},
    freeVariableReplacementRules = Reverse /@ Select[replacementRules, MatchQ[#,Rule[_Symbol, C[_]]]&];
	replacementRules /. freeVariableReplacementRules
  ]


EquationSystem[template1_List,template2_List]:=
  Equal @@ # & /@ Transpose[{template1, template2}];


ReplacementRules[template1_, template2_, k_Integer:0]:=
  With[{
      templateVars = SortBy[Union[Flatten[RuleTemplateVars[#] & /@ {template1, template2}, 1]], FromDigits[StringDrop[SymbolName[#],1]]&]
    },
    If[k === 0,
      Quiet[Solve[EquationSystem[template1, template2], templateVars]],
      Quiet[Solve[EquationSystem[template1, template2], Reverse[templateVars], Modulus -> k]]]];


End[];
EndPackage[];
