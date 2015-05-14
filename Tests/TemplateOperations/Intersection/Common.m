(* ::Package:: *)

<< CATemplates`


Print[ConstantsToVariables[{x0 -> C[1]}] === {x0 -> x0}]


Print[ConstantsToVariables[{x1 -> C[2], x0 -> C[1]}] === {x1 -> x1, x0 -> x0}]


Print[ConstantsToVariables[{x2 -> 1 + C[1], x1 -> C[2], x0 -> C[1]}] === {x2 -> 1 + x0, x1 -> x1, x0 -> x0}]


Print[ConstantsToVariables[{x3 -> 1 + C[1] + C[2], x2 -> 1 + C[1], x1 -> C[2], x0 -> C[1]}] == {x3 -> 1 + x0 + x1, x2 -> 1 + x0,   x1 -> x1, x0 -> x0}]
