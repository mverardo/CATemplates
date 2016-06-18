BeginPackage["CATemplates`TemplateOperations`Expansion`PostExpansionFn`FilterNotAllowed`", {"CATemplates`CATemplate`"}];

FilterNotAllowed::usage="FilterNotAllowed[t_, expansion_] = Returns expansion if it has only elements dictated by the template's variable restrictions. Otherwise, returns the vanishing function (##) & []";

Begin["`Private`"];

(* Propagates the Vanish function upwards if it receives one. *)
FilterNotAllowed[template_Association, Sequence[]] := (##) & [];

FilterNotAllowed[template_Association, expansion_List] :=
    With[{memberQsInHold = Cases[expansion, (value_ \[Element] set_List -> Hold[MemberQ[set, value]]), Infinity]},
      If[And @@ (ReleaseHold /@ memberQsInHold) == False,
        (##) & [],  (* At least one of the expressions is invalid, this expansion should be filtered *)
        expansion /. (value_ \[Element] set_List -> value)]]; (* All expressions are valid, this expansion is ok. *)

End[];

EndPackage[];
