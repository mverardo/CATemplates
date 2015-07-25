(* ::Package:: *)

<< CATemplates`;


With[
  {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "rawList" -> {x7, x6, x5, x4, x3, x2, x1, x0}, "expansionFunction" -> Identity|>},
  Print[BuildTemplate[2, 1.0, {x7, x6, x5, x4, x3, x2, x1, x0}, Identity] === expectedTemplate]
]

With[
  {expectedTemplate = <|"k" -> 2, "r" -> 1.0, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Identity|>},
  Print[BuildTemplate[2, 1.0, {x7, 1, x5, x4, 1, x2, x1, 0}, Identity] === expectedTemplate]
]

With[
  {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2]|>},
  Print[BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2]] === expectedTemplate]
]

With[
  {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2], "N" -> 2|>},
  Print[BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 2] === expectedTemplate]
]

With[
  {expectedTemplate = <|"k" -> 3, "r" -> 1.5, "rawList" -> {x7, 1, x5, x4, 1, x2, x1, 0}, "expansionFunction" -> Function[{x,y}, 2], "N" -> 3|>},
  Print[BuildTemplate[3, 1.5, {x7, 1, x5, x4, 1, x2, x1, 0}, Function[{x,y}, 2], 3] === expectedTemplate]
]