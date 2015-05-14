(* ::Package:: *)

<<CATemplates`


Print[ModularTemplateIntersection[{x1, x0}, {x1, x0}, 3] === {x1, x0}]


Print[ModularTemplateIntersection[{x1, x0}, {1, x0}, 3] === {1, x0}]


Print[ModularTemplateIntersection[{x2, x1, x0}, {x2, 1, x0}, 3] === {x2, 1, x0}]


Print[
With[
 {i1 = ModularTemplateIntersection[{x1, x0}, {1, x0}, 3],
  i2 = ModularTemplateIntersection[{x1, x0}, {0, x0}, 3]},
 ModularTemplateIntersection[i1, i2, 3] === {}]
]


Print[
With[
 {i1 = ModularTemplateIntersection[{x1, x0}, {1, x0}, 3],
  i2 = {x1, 0}},
 ModularTemplateIntersection[i1, i2, 3] === {1, 0}]
]
