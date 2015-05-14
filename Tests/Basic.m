(* ::Package:: *)

<<CATemplates`


Print["RawTemplate"]


Print[RawTemplate[BaseTemplate[]] === BaseTemplate[]]


Print[RawTemplate[{x8, 0, x6, 0, x4, 0, x2, 0}] === {x8, 0, x6, 0, x4, 0, x2, 0}]


Print[RawTemplate[{x2 \[Element] {0, 2}, x0, x0}] === {x2, x0, x0}]


Print[RawTemplate[{x2, x1 \[Element] {0, 2}, x0}] === {x2, x1, x0}]


Print["ImprisonmentExpressions"]


Print[ImprisonmentExpressions[{x8, 0, x6, 0, x4, 0, x2, 0}] == {}]


Print[ImprisonmentExpressions[{x2 \[Element] {0, 2}, x0, x0}]  === {x2 \[Element] {0, 2}}]


Print[ImprisonmentExpressions[{x2, x1 \[Element] {0, 2}, x0}] === {x1 \[Element] {0, 2}}]


Print[ImprisonmentExpressions[{x2 \[Element] {0, 1}, x1 \[Element] {0, 2}, x0}] === {x2 \[Element] {0, 1}, x1 \[Element] {0, 2}}]


Print["Value Restrictions"]


Print[ValueRestrictions[x1 \[Element] {0, 1}] === (x1 == 0 || x1 == 1)]


Print[ValueRestrictions[x2 \[Element] {0, 1}] === (x2 == 0 || x2 == 1)]


Print[ValueRestrictions[x2 \[Element] {0, 1, 2}] === (x2 == 0 || x2 == 1 || x2 == 2)]


Print["ExceptionTemplates"]


Print[ExceptionTemplates[{x7, x6, x5, x4, x3, x2, x1, x0}] === {}]


Print[ExceptionTemplates[{x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x2 - x1}, 3] === {{x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 1, x0}, {x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 2, x0}, {x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 1, 2, x0}}]


Print[ExceptionTemplates[{x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x2 - x1}, 2, 2] === {{x31, x30, x29, x28, x27, x26, x25, x24, x23, x22, x21, x20, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, 0, 1, x0}}]
