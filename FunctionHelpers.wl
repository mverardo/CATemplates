BeginPackage["CATemplates`FunctionHelpers`"];

Partial::usage = "Partial[f_, args__] := partially applies arguments args to function f.";

Begin["`Private`"];

SetAttributes[Partial, HoldAll];
Partial[f_, as__] := Function[Null, f[as, ##], HoldAll];

End[];

EndPackage[];