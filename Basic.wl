(* ::Package:: *)

BeginPackage["CATemplates`Basic`"];


Partial::usage = "Partial[f_, args__] := partially applies arguments args to function f.";

PrintTestResults::usage = "PrintTestResults[testReport_] := Prints the results of a testReport in a terminal friendly manner";

RawTemplate::usage="RawTemplate[t_List]: Receives a template t, and drops any special sintax construct from it. Currently, it removes expressions of the form x \[Element] {__}.";


ImprisonmentExpressions::usage="ImprisonmentExpressions[t_List]: Receives a template t, and returns all of the expressions of the form x \[Element] {__}.";


ValueRestrictions::usage = "ValueRestrictions[imprisonmentExpression_]: Returns an expression that represents the value restricions dictated by an ImprisonmentExpression. Example: ValueRestrictions[x1 \[Element] {0,1}] -> x1 == 0 || x1 == 1";


FreeVariableQ::usage = "FreeVariableQ[expression_]: Receives an expression, and returns true if the expression is a free variable and false otherwise.";


CorrespondsToNeighborhoodQ::usage = "CorrespondsToNeighborhoodQ[freeVariable_Symbol, nbIndex_Integer]: Receives a free variable expression and a neighborhood index, and returns true if the variable's index corresponds to the received nb index.";


PreservesIndexVariableDualityQ::usage = "PreservesIndexVariableDualityQ[template_]: Receives a template and returns true if the template preserver the index-variable diality.";


ConstantsToVariables::usage = "ConstantsToVariables[replacementRules_]: Receives a list of replacement rules, and converts any symbol of the type C[i_Integer] into its corresponding template variable, preserving the index-variable duality."

Begin["`Private`"];

SetAttributes[Partial, HoldAll];
Partial[f_, as__] := Function[Null, f[as, ##], HoldAll];

(*Deprecated!!*)
RuleTemplateVars[ruletemplate_Association] :=
    RuleTemplateVars[ruletemplate[["core"]]];
(*Deprecated!!*)
RuleTemplateVars[ruletemplate_] :=
  SortBy[Union[Cases[ruletemplate, _Symbol, Infinity]], FromDigits[StringDrop[SymbolName[#],1]]&]


TemplateVarFromNeighbourhood[neighbourhood_List, k_Integer: 2] :=
  Symbol["x" <> ToString@FromDigits[neighbourhood, k]];


RawTemplate[template_]:= template /. Element[x_,set_] -> x;


ImprisonmentExpressions[template_List]:= Cases[template, x_ \[Element] set_ ,Infinity]


ValueRestrictions[imprisonmentExpression_]:=
 Apply[Or,imprisonmentExpression[[1]] == #&/@ imprisonmentExpression[[2]]];


FreeVariableQ[expression_] := MatchQ[expression, _Symbol];


CorrespondsToNeighborhoodQ[symbol_, nbIndex_] := 
  (FromDigits[StringDrop[SymbolName[symbol], 1]] === nbIndex);


PreservesIndexVariableDualityQ[template_] :=
  And @@ (MapIndexed[(!FreeVariableQ[#1]) || (CorrespondsToNeighborhoodQ[#1, First[#2] - 1]) &, Reverse[template]]);


ConstantsToVariables[replacementRules_List] := 
  Module[{freeVariableReplacementRules},
	freeVariableReplacementRules = Reverse /@ Select[Sort[replacementRules], MatchQ[#,Rule[_Symbol, C[_]]]&];
	replacementRules /. freeVariableReplacementRules
  ]


PrintTestResults[testReport_] :=
    Module[{red = "\033[0;31m", green = "\033[0;32m", noColor = "\033[0m"},
      Print[green <> "Suceeded: " <> ToString[testReport["TestsSucceededCount"]] <> noColor];
      If[testReport["TestsFailedCount"] > 0,
        Print[red <> "Failed: " <> ToString[testReport["TestsFailedCount"]] <> noColor]];
    ];

End[];
EndPackage[];
