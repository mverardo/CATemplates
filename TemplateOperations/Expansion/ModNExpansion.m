(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: ModNExpansion     *)
(* :Context: ModNExpansion`  *)
(* :Author: mverardo            *)
(* :Date: 26/07/15              *)

(* :Package Version: 1.0       *)
(* :Mathematica Version:       *)
(* :Copyright: (c) 2015 mverardo *)
(* :Keywords:                  *)
(* :Discussion:                *)

BeginPackage["CATemplates`TemplateOperations`Expansion`ModNExpansion`", {"CATemplates`Basic`", "CATemplates`TemplateOperations`Expansion`RawExpansion`"}];

ModNExpansion::usage =
"ModNExpansion[T_Template] := performs a full expansion on template T, and aaplies mod N to the result (N being a property of the template).
ModNExpansion[T_Template, i_Integer] := performs the ith expansion on template T. Applies the mod N on the result (N being a property of the template).";

Begin["`Private`"]; (* Begin Private Context *)

ModNExpansion[template_Association, i_Integer] :=
    With[
      {expansionFunction = Composition[Mod[#, template[["N"]]] &, RawExpansion]},
      expansionFunction[template, i]
    ];

ModNExpansion[template_Association] :=
    ModNExpansion[template, #] & /@ SubstitutionRange[template];

End[];

EndPackage[];