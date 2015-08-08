(* ::Package:: *)

<< CATemplates`


Print["Raw Intersection"]


Print[TemplateIntersection[ConstantArray[0, 8], ConstantArray[1, 8]] === {}]


Print[TemplateIntersection[{x8, 0, x6, 0, x4, 0, x2, 0}, {x8, 0, x6, 0, x4, 0, x2, 0}] === {{x8, 0, x6, 0, x4, 0, x2, 0}}]


Print[TemplateIntersection[OldBaseTemplate[], OldBaseTemplate[]] === {OldBaseTemplate[]}]


Print[TemplateIntersection[OldBaseTemplate[2, 2], OldBaseTemplate[2, 2]] === {OldBaseTemplate[2, 2]}]


Print[TemplateIntersection[OldBaseTemplate[2, 3], OldBaseTemplate[2, 3]] === {OldBaseTemplate[2, 3]}]


Print[TemplateIntersection[OldBaseTemplate[], ConstantArray[0, 8]] === {ConstantArray[0, 8]}]


Print[TemplateIntersection[OldBaseTemplate[], ConstantArray[1, 8]] === {ConstantArray[1, 8]}]


Print[TemplateIntersection[OldBaseTemplate[], {x7, 0, x5, 0, x3, 0, x1, 0}] === {{x7, 0, x5, 0, x3, 0, x1, 0}}]


Print[TemplateIntersection[{x7, 0, x5, 0, x3, 0, x1, 0}, OldBaseTemplate[]] === {{x7, 0, x5, 0, x3, 0, x1, 0}}]


Print["Variable Constraint Intersection"]


Print[TemplateIntersection[{x2 \[Element] {0, 2}, x1, x0}, {x2 \[Element] {0, 2}, x0, x0}] === {x2 \[Element] {0, 2}, x0, x0}]


Print[TemplateIntersection[{x2, x1, x0}, {x2 \[Element] {0, 2}, x0, x0}] === {x2 \[Element] {0, 2}, x0, x0}]


Print[TemplateIntersection[{x2 \[Element] {0, 2}, x1, x0}, {x2 \[Element] {0, 1}, x0, x0}] === {0, x0, x0}]


Print[TemplateIntersection[{x2, x1 \[Element] {0, 2}, x0}, {x2 \[Element] {0, 1}, x0, x0}] === {x2 \[Element] {0, 1}, x0 \[Element] {0, 2}, x0 \[Element] {0, 2}}]


Print[TemplateIntersection[{x2 \[Element] {0, 2}, x1, x0}, {0, x0, x0}] === {0, x0, x0}]


Print[TemplateIntersection[{x2 \[Element] {2}, x1, x0}, {2, x0, x0}] === {2, x0, x0}]


Print[TemplateIntersection[{x2 \[Element] {2}, x1, x0}, {0, x0, x0}] === {}]


Print[TemplateIntersection[{x2 \[Element] {2, 3}, x1, x0}, {x2 \[Element] {1, 2, 3}, x0, x0}] === {x2 \[Element] {2, 3}, x0, x0}]


Print[TemplateIntersection[{x2 \[Element] {0, 2}, x1 \[Element] {0, 1}, x0}, {x2 \[Element] {0, 1}, x0 \[Element] {1, 2}, x0}] === {0, 1, 1}]
