BeginPackage["CATemplates`TemplateOperations`Expansion`PostExpansionFn`IdentityFn`"];

IdentityFn::usage="Identity[t_, expansion_] := expansion";

Begin["`Private`"];

IdentityFn[template_Association, expansion_List] := expansion;

End[];

EndPackage[];