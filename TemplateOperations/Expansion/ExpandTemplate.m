(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: ExpandTemplate     *)
(* :Context: ExpandTemplate`  *)
(* :Author: mverardo            *)
(* :Date: 26/07/15              *)

BeginPackage["CATemplates`TemplateOperations`Expansion`ExpandTemplate`",
  {
    "CATemplates`Basic`",
    "CATemplates`CATemplate`"
  }];

ExpandTemplate::usage =
"ExpandTemplate[T_Template] := performs a full expansion on template T, using the template's expansion function.
ExpandTemplate[T_Template, i_Integer] := performs the ith expansion on template T, using the template's expansion function.";

Begin["`Private`"];

Substition[i_, k_, variables_] :=
    Reverse[IntegerDigits[i, k, Length[variables]]];

TransformationRules[variables_, substitution_] :=
    MapThread[#1 -> #2 &, {variables, substitution}];

Expansion[template_Association, i_Integer] :=
    With[{k = k[template], variables = RuleTemplateVars[template]},
      kAryRuleTemplate[template] /. TransformationRules[variables, Substitution[i, k, variables]]];

ExpandTemplate[template_Association, i_Integer] :=
    If[postExpansionFn[t] === Null,
      Expansion[template, i],
      postExpansionFn[t] @ Expansion[template, i]];

ExpandTemplate[template_Association] :=
    ExpandTemplate[template, #] & /@ SubstitutionRange[template];

End[];

EndPackage[];
