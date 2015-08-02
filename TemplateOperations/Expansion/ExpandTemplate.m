(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: ExpandTemplate     *)
(* :Context: ExpandTemplate`  *)
(* :Author: mverardo            *)
(* :Date: 26/07/15              *)

BeginPackage["CATemplates`TemplateOperations`Expansion`ExpandTemplate`", {"CATemplates`Basic`"}];

ExpandTemplate::usage =
"ExpandTemplate[T_Template] := performs a full expansion on template T, using the template's expansion function.
ExpandTemplate[T_Template, i_Integer] := performs the ith expansion on template T, using the template's expansion function.";

Begin["`Private`"];

ExpandTemplate[template_Association, i_Integer] :=
    template[["expansionFunction"]][template, i];


ExpandTemplate[template_Association] :=
    template[["expansionFunction"]][template];

End[];

EndPackage[];