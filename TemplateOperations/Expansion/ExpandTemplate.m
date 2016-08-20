BeginPackage["CATemplates`TemplateOperations`Expansion`ExpandTemplate`",
  {
    "CATemplates`CATemplate`",
    "CATemplates`FunctionHelpers`"
  }];

ExpandTemplate::usage =
"ExpandTemplate[T_Template] := performs a full expansion on template T, using the template's expansion function.
ExpandTemplate[T_Template, i_Integer] := performs the ith expansion on template T, using the template's expansion function.";

Begin["`Private`"];

SubstitutionRange[template_Association] :=
    If[templateCore[template] === {},
      {},
      Range[0, (template[["k"]]^Length[TemplateCoreVars[template]])-1]];

Substitution[i_, k_, variables_] :=
    Reverse[IntegerDigits[i, k, Length[variables]]];

TransformationRules[variables_, substitution_] :=
    MapThread[#1 -> #2 &, {variables, substitution}];

Expansion[template_Association, i_Integer] :=
    With[{variables = TemplateCoreVars[template]},
      templateCore[template] /. TransformationRules[variables, Substitution[i, k[template], variables]]];

(* Takes a list of 2 args Functions, fixes their firstArgument as firstArgument and composes a new one-arg Function. *)
PartialComposition[postExpansionFns_List, firstArgument_] :=
    Fold[Composition, Partial[#, firstArgument] & /@ postExpansionFns];

(* If there is no postExpansionFn: *)
PostExpansion[template_Association, i_Integer, postExpansionFn_ /; MissingQ[postExpansionFn]] :=
    Expansion[template, i];

(* If there is one postExpansionFn: *)
PostExpansion[template_Association, i_Integer, postExpansionFn_Symbol] :=
    Partial[postExpansionFn, template] @ Expansion[template, i];

(* If there is a list of postExpansionFns: *)
PostExpansion[template_Association, i_Integer, postExpansionFns_List] :=
    PartialComposition[postExpansionFns, template] @ Expansion[template, i];

ExpandTemplate[template_Association, i_Integer] :=
    PostExpansion[template, i, postExpansionFn[template]];

ExpandTemplate[template_Association] :=
    ExpandTemplate[template, #] & /@ SubstitutionRange[template];

End[];

EndPackage[];
