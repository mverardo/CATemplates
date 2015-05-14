(* ::Package:: *)

<<CATemplates`


Print[ExpandTemplateModK[{}, 2] === {}]


Print[ExpandTemplateModK[{2 - x0, x0}, 2] === {{0, 0}, {1, 1}}]


Print[ExpandTemplateModK[{3 - x0, x0}, 2] === {{1, 0}, {0, 1}}]


Print[ExpandTemplateModK[{3 - x0, x0}, 3] === {{0, 0}, {2, 1}, {1, 2}}]


Print[ExpandTemplateModK[{1 + x2, x2, 1 + x0, x0}, 3] === {{1, 0, 1, 0}, {1, 0, 2, 1}, {1, 0, 0, 2}, {2, 1, 1, 0}, {2, 
   1, 2, 1}, {2, 1, 0, 2}, {0, 2, 1, 0}, {0, 2, 2, 1}, {0, 2, 0, 2}}]

