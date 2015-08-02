(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

BeginPackage["CATemplates`TemplateOperations`Expansion`RawExpansion`", "CATemplates`Basic`"];

RawExpansion::usage =
    "RawExpansion[T_Template] := performs a full expansion on template T, and leaves the result as it is.
RawExpansion[T_Template, i_Integer] := performs the ith expansion on template T, and leaves the result as it is.";

Begin["`Private`"];

RawExpansion[template_Association, i_Integer] :=
    Module[
      {
        k = template[["k"]],
        r = template[["r"]],
        variables = RuleTemplateVars[template],
        substitution,
        transformationRules
      },
      substitution = Reverse[IntegerDigits[i, k, Length[variables]]];
      transformationRules = MapThread[#1 -> #2 &, {variables, substitution}];
      template[["rawList"]] /. transformationRules
    ];

RawExpansion[template_Association] :=
    RawExpansion[template, #] & /@ SubstitutionRange[template];

End[];
EndPackage[];