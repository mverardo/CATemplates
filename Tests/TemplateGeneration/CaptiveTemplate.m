(* ::Package:: *)

<< CATemplates`;

CaptiveQ[karyTable_, k_, r_] :=
    With[{ruleTable = RuleTableFromKAry[karyTable, k, r]},
      And @@ (MemberQ[#[[1]], #[[2]]] & /@ ruleTable)];

report = TestReport[{
  VerificationTest[
    CaptiveTemplate[2, 1.0] === BuildTemplate[2, 1.0, {1, x6, x5, x4, x3, x2, x1, 0}, {FilterNotAllowed, FilterOutOfRange}]],
  VerificationTest[
    And @@ (CaptiveQ[#, 2, 1.0] & /@ ExpandTemplate[CaptiveTemplate[2, 1.0]]) === True],
  VerificationTest[
    CaptiveTemplate[3, 1.0] === BuildTemplate[3, 1.0, {2, x25 \[Element] {1, 2},
      x24 \[Element] {0, 2}, x23 \[Element] {1, 2}, x22 \[Element] {1, 2},
      x21, x20 \[Element] {0, 2}, x19, x18 \[Element] {0, 2},
      x17 \[Element] {1, 2}, x16 \[Element] {1, 2}, x15,
      x14 \[Element] {1, 2}, 1, x12 \[Element] {0, 1}, x11,
      x10 \[Element] {0, 1}, x9 \[Element] {0, 1}, x8 \[Element] {0, 2},
      x7, x6 \[Element] {0, 2}, x5, x4 \[Element] {0, 1},
      x3 \[Element] {0, 1}, x2 \[Element] {0, 2}, x1 \[Element] {0, 1}, 0}, {FilterNotAllowed, FilterOutOfRange}]],
  VerificationTest[
    CaptiveTemplate[3, 2.0] === BuildTemplate[3, 2.0,
      {2, x241 \[Element] {1, 2},
        x240 \[Element] {0, 2}, x239 \[Element] {1, 2},
        x238 \[Element] {1, 2}, x237, x236 \[Element] {0, 2}, x235,
        x234 \[Element] {0, 2}, x233 \[Element] {1, 2},
        x232 \[Element] {1, 2}, x231, x230 \[Element] {1, 2},
        x229 \[Element] {1, 2}, x228, x227, x226, x225,
        x224 \[Element] {0, 2}, x223, x222 \[Element] {0, 2}, x221, x220,
        x219, x218 \[Element] {0, 2}, x217, x216 \[Element] {0, 2},
        x215 \[Element] {1, 2}, x214 \[Element] {1, 2}, x213,
        x212 \[Element] {1, 2}, x211 \[Element] {1, 2}, x210, x209, x208,
        x207, x206 \[Element] {1, 2}, x205 \[Element] {1, 2}, x204,
        x203 \[Element] {1, 2}, x202 \[Element] {1, 2}, x201, x200, x199,
        x198, x197, x196, x195, x194, x193, x192, x191, x190, x189,
        x188 \[Element] {0, 2}, x187, x186 \[Element] {0, 2}, x185, x184,
        x183, x182 \[Element] {0, 2}, x181, x180 \[Element] {0, 2}, x179,
        x178, x177, x176, x175, x174, x173, x172, x171,
        x170 \[Element] {0, 2}, x169, x168 \[Element] {0, 2}, x167, x166,
        x165, x164 \[Element] {0, 2}, x163, x162 \[Element] {0, 2},
        x161 \[Element] {1, 2}, x160 \[Element] {1, 2}, x159,
        x158 \[Element] {1, 2}, x157 \[Element] {1, 2}, x156, x155, x154,
        x153, x152 \[Element] {1, 2}, x151 \[Element] {1, 2}, x150,
        x149 \[Element] {1, 2}, x148 \[Element] {1, 2}, x147, x146, x145,
        x144, x143, x142, x141, x140, x139, x138, x137, x136, x135,
        x134 \[Element] {1, 2}, x133 \[Element] {1, 2}, x132,
        x131 \[Element] {1, 2}, x130 \[Element] {1, 2}, x129, x128, x127,
        x126, x125 \[Element] {1, 2}, x124 \[Element] {1, 2}, x123,
        x122 \[Element] {1, 2}, 1, x120 \[Element] {0, 1}, x119,
        x118 \[Element] {0, 1}, x117 \[Element] {0, 1}, x116, x115, x114,
        x113, x112 \[Element] {0, 1}, x111 \[Element] {0, 1}, x110,
        x109 \[Element] {0, 1}, x108 \[Element] {0, 1}, x107, x106, x105,
        x104, x103, x102, x101, x100, x99, x98, x97, x96, x95,
        x94 \[Element] {0, 1}, x93 \[Element] {0, 1}, x92,
        x91 \[Element] {0, 1}, x90 \[Element] {0, 1}, x89, x88, x87, x86,
        x85 \[Element] {0, 1}, x84 \[Element] {0, 1}, x83,
        x82 \[Element] {0, 1}, x81 \[Element] {0, 1}, x80 \[Element] {0, 2},
        x79, x78 \[Element] {0, 2}, x77, x76, x75, x74 \[Element] {0, 2},
        x73, x72 \[Element] {0, 2}, x71, x70, x69, x68, x67, x66, x65, x64,
        x63, x62 \[Element] {0, 2}, x61, x60 \[Element] {0, 2}, x59, x58,
        x57, x56 \[Element] {0, 2}, x55, x54 \[Element] {0, 2}, x53, x52,
        x51, x50, x49, x48, x47, x46, x45, x44, x43, x42, x41,
        x40 \[Element] {0, 1}, x39 \[Element] {0, 1}, x38,
        x37 \[Element] {0, 1}, x36 \[Element] {0, 1}, x35, x34, x33, x32,
        x31 \[Element] {0, 1}, x30 \[Element] {0, 1}, x29,
        x28 \[Element] {0, 1}, x27 \[Element] {0, 1}, x26 \[Element] {0, 2},
        x25, x24 \[Element] {0, 2}, x23, x22, x21, x20 \[Element] {0, 2},
        x19, x18 \[Element] {0, 2}, x17, x16, x15, x14,
        x13 \[Element] {0, 1}, x12 \[Element] {0, 1}, x11,
        x10 \[Element] {0, 1}, x9 \[Element] {0, 1}, x8 \[Element] {0, 2},
        x7, x6 \[Element] {0, 2}, x5, x4 \[Element] {0, 1},
        x3 \[Element] {0, 1}, x2 \[Element] {0, 2}, x1 \[Element] {0, 1}, 0}, {FilterNotAllowed, FilterOutOfRange}]]}];

PrintReport[report];
