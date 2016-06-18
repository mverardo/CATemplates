BeginPackage["CATemplates`TemplateOperations`Expansion`PostExpansionFn`TemplateMod`", {"CATemplates`CATemplate`"}];

TemplateMod::usage="TemplateMod[t_, expansion_] = Performs a mod N on every element of the expansion (N being a template property).";

Begin["`Private`"];

TemplateMod[template_Association, expansion_List] :=
    Mod[expansion, templateMod[template]];

End[];

EndPackage[];
