(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 25/07/15 *)

BeginPackage["CATemplates`TemplateOperations`Expansion`FilteredExpansion`", {"CATemplates`Basic`", "CATemplates`TemplateOperations`Expansion`RawExpansion`"}];

FilteredExpansion::usage =
"FilteredExpansion[T_Template] := performs a full expansion on template T, and filters any invalid result.
FilteredExpansion[T_Template, i_Integer] := performs the ith expansion on template T. Returns null if it yields an invalid substitution.";

Begin["`Private`"];

ValidatedExpansion[k_Integer, expansion_List] :=
  If[SubsetQ[Range[0, k - 1], Union[Cases[expansion, x_Integer, 2]]],
    expansion,
    (* If the expansion is supposed to be filtered, we return the special ##&[] function, which vanishes once evaluated.
         More info on:
         http://stackoverflow.com/questions/8877324/how-to-remove-the-null-symbol-in-a-table-in-mathematica *)
    (##) & []
  ];


FilteredExpansion[template_Association, i_Integer] :=
    With[
      {expansionFunction = Composition[Partial[ValidatedExpansion, template[["k"]]], RawExpansion]},
      expansionFunction[template, i]
    ];

FilteredExpansion[template_Association] :=
    FilteredExpansion[template, #] & /@ SubstitutionRange[template];

End[];
EndPackage[];