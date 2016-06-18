BeginPackage["CATemplates`TemplateOperations`Expansion`PostExpansionFn`ModK`", {"CATemplates`CATemplate`"}];

ModK::usage="Performs a mod K on every element of the expansion.";

Begin["`Private`"];

ModK[template_Association, expansion_List] :=
    Mod[expansion, k[template]];

End[];

EndPackage[];
