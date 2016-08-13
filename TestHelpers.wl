BeginPackage["CATemplates`TestHelpers`"];

PrintReport::usage = "PrintReport[testReport_] := Prints the results of a testReport in a terminal friendly manner";

Begin["`Private`"];

PrintReport[testReport_] :=
    Module[{red = "\033[0;31m", green = "\033[0;32m", noColor = "\033[0m"},
      Print[green <> "Suceeded: " <> ToString[testReport["TestsSucceededCount"]] <> noColor];
      If[testReport["TestsFailedCount"] > 0,
        Print[red <> "Failed: " <> ToString[testReport["TestsFailedCount"]] <> noColor]];
    ];

End[];

EndPackage[];