(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: ModKExpansion     *)
(* :Context: ModKExpansion`  *)
(* :Author: mverardo            *)
(* :Date: 26/07/15              *)

(* :Package Version: 1.0       *)
(* :Mathematica Version:       *)
(* :Copyright: (c) 2015 mverardo *)
(* :Keywords:                  *)
(* :Discussion:                *)

BeginPackage["CATemplates`TemplateOperations`Expansion`ModKExpansion`", {"CATemplates`Basic`", "CATemplates`TemplateOperations`Expansion`RawExpansion`"}]
(* Exported symbols added here with SymbolName::usage *)

ModKExpansion::usage =
"ModKExpansion[T_Template] := performs a full expansion on template T, and applies mod k to the result (k being the number of states of the CAs represented by the template).
ModKExpansion[T_Template, i_Integer] := performs the ith expansion on template T. Applies the mod N on the result (k being the number of states of the CAs represented by the template).";

Begin["`Private`"];

ModKExpansion[template_Association, i_Integer] :=
    With[
      {expansionFunction = Composition[Mod[#, template[["k"]]] &, RawExpansion]},
      expansionFunction[template, i]
    ];

ModKExpansion[template_Association] :=
    ModKExpansion[template, #] & /@ SubstitutionRange[template];

End[];

EndPackage[];