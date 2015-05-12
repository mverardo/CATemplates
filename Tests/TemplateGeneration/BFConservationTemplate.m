(* ::Package:: *)

<< CATemplates`


Print[BFConservationTemplate[]==={1,1+x2-x3,1-x2,1-x1-x2,x3,x2,x1,0}]


Print[BFConservationTemplate[3,1]==={2,2+x7-x8,2+x6-x8,2+x1-x2+x5-x7,2+x1-x2+x4-x7,2+x1-x2+x3-x7,2-x6,2+x1-x2-x6,2-x2-x6,1-x1+x2-x5+x8,1-x1+x2-x5+x7,1-x1+x2-x5+x6,1-x4+x5,1,1+x3-x4,1-x1+x2-x3,1-x3,1-x1-x3,x8,x7,x6,x5,x4,x3,x2,x1,0}]


Print[BFConservationTemplate[2,2]==={1,1+x14-x15,1+x13-x14+x6-x7,1+x12-x14+x6-x7,1+x11-x13+x2-x3+x5-x6,1+x10-x13+x2-x3+x5-x6,1-x12+x2-x3+x4-x6+x9,1-x12+x2-x3+x4-x6+x8,1-x11-x2+x3-x5+x7,1-x11-x2+x3-x5+x6,1-x10,1-x10+x4-x5,1-x2+x3-x4-x9,1-x4-x9,1-x2-x4-x8,1-x1-x2-x4-x8,x15,x14,x13,x12,x11,x10,x9,x8,x7,x6,x5,x4,x3,x2,x1,0}]


Print[ExpandTemplate[BFConservationTemplate[]]=={{1,1,1,1,0,0,0,0},{1,1,1,0,0,0,1,0},{1,0,1,1,1,0,0,0},{1,0,1,0,1,0,1,0},{1,1,0,0,1,1,0,0}}]
