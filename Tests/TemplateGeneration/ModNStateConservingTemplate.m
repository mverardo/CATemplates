(* ::Package:: *)

<<CATemplates`


Print[
  Sort[FromDigits[#,2] & /@ ExpandTemplateModK[ModNStateConservingTemplate[2], 2]] == {132,150,170,184,204,222,226,240}
]


Print[
  PreservesIndexVariableDualityQ[ModNStateConservingTemplate[2]]
]


(*Print[
  Sort[FromDigits[#,2] & /@ ExpandTemplateModK[ModNStateConservingTemplate[3], 2]] == {170, 184, 204, 226, 240}
]*)


Print[
  PreservesIndexVariableDualityQ[ModNStateConservingTemplate[3]]
]
