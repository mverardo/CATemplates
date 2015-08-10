(* ::Package:: *)

<< CATemplates`;
<< CATemplates`TemplateOperations`ExpandTemplate`;

Print[ExpandTemplate[{}] === {}]


Print[ExpandTemplate[OldBaseTemplate[]] === Tuples[{0, 1}, 8]]


Print[ExpandTemplate[OldBaseTemplate[], 2, 0] === {0, 0, 0, 0, 0, 0, 0, 0}]


Print[ExpandTemplate[OldBaseTemplate[], 2, 4] === {0, 0, 0, 0, 0, 1, 0, 0}]


Print[ExpandTemplate[{0, 1, x1, 0, 1}] === {{0, 1, 0, 0, 1}, {0, 1, 1, 0, 1}}]


Print[ExpandTemplate[{0, 2, x1, 0, 1}, 3] === {{0, 2, 0, 0, 1}, {0, 2, 1, 0, 1}, {0, 2, 2, 0, 1}}]



Print[ExpandTemplate[{0, 2, x1 \[Element] {0, 2}, 0, 1}, 3] === {{0, 2, 0, 0, 1}, {0, 2, 2, 0, 1}}]


Print[ExpandTemplate[{0, 2, x1 \[Element] {1, 2}, 0, 1}, 3] === {{0, 2, 1, 0, 1}, {0, 2, 2, 0, 1}}]


Print[ExpandTemplate[{0, 2, x1 \[Element] {0, 1}, 0, 1}, 3] === {{0, 2, 0, 0, 1}, {0, 2, 1, 0, 1}}]


Print[ExpandTemplate[{0, 1, 0, 2, x1}, 3] === {{0, 1, 0, 2, 0}, {0, 1, 0, 2, 1}, {0, 1, 0, 2, 2}}]


Print[ExpandTemplate[OldBaseTemplate[], 2, {0, 4}] === Tuples[{0, 1}, 8][[1 ;; 5]]]


Print[ExpandTemplate[{0, x1, x2 \[Element] {0, 1}, 2 - x3}] === {{0, 0, 0, 1}, {0, 1, 0, 1}, {0, 0, 1, 1}, {0, 1, 1, 1}}]
