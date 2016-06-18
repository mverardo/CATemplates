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

Substitution[i_, k_, variables_] :=
    Reverse[IntegerDigits[i, k, Length[variables]]];

TransformationRules[variables_, substitution_] :=
    MapThread[#1 -> #2 &, {variables, substitution}];

Expansion[template_Association, i_Integer] :=
    With[{variables = RuleTemplateVars[template]},
      kAryRuleTemplate[template] /. TransformationRules[variables, Substitution[i, k[template], variables]]];

ExpandTemplate[template_Association, i_Integer] :=
    If[MissingQ[postExpansionFn[template]],
      Expansion[template, i],
      Partial[postExpansionFn[template], template] @ Expansion[template, i]];

ExpandTemplate[template_Association] :=
    ExpandTemplate[template, #] & /@ SubstitutionRange[template];

End[];

EndPackage[];
