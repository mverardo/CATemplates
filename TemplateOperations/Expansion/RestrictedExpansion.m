BeginPackage["CATemplates`TemplateOperations`Expansion`RestrictedExpansion`", {"CATemplates`Basic`", "CATemplates`TemplateOperations`Expansion`FilteredExpansion`"}];

RestrictedExpansion::usage=
"RestrictedExpansion[T_template] := Performs a full expansion on template T, filtering any results that don't conform to the T's restriction expressions.
RestrictedExpansion[T_template, i_Integer] := Performs the ith expansion template T. Returns null if the expansion doesn't conform to the templates restriction expressions.";

Begin["`Private`"];


(* Propagates the empty sequence upwards if it receives one. *)
RestrictExpansion[Sequence[]] := (##) & [];

RestrictExpansion[expansion_List] :=
    With[{memberQsInHold = Cases[expansion, (value_ \[Element] set_List -> Hold[MemberQ[set, value]]), Infinity]},
      If[And @@ (ReleaseHold /@ memberQsInHold) == False,
        (##) & [], (* At least one of the expressions is invalid, this expansion should be filtered *)
        expansion /. (value_ \[Element] set_List -> value) (* All expressions are valid, this expansion is ok. *)
      ]
    ];


RestrictedExpansion[template_Association, i_Integer] :=
    With[
      {expansionFunction = Composition[RestrictExpansion, FilteredExpansion]},
      expansionFunction[template, i]
    ];

RestrictedExpansion[template_Association] :=
    RestrictedExpansion[template, #] & /@ SubstitutionRange[template];


End[];


EndPackage[];
