(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`SymmetricTemplate`", "CATemplates`Basic`"];


TotalisticTemplate::usage="Returns a template that represents all of the totalistic rules in a given space (defined by k and r).";


OuterTotalisticTemplate::usage="Returns a template that represents all of the outer totalistic rules in a given space (defined by k and r).";


Begin["`Private`"];


TotalisticTemplate[k_Integer: 2, r_Integer: 1] :=
 Module[
  {neighbourhoodsSums, symbolSubscripts, lastNeighborhoodsWithValue},
  (*Calculo a soma de cada vizinhan\[CCedilla]a do espa\[CCedilla]o, 
  e crio uma lista {vizinhan\[CCedilla]a, soma}*)
  
  neighbourhoodsSums = {#, Plus @@ #} & /@ AllNeighbourhoods[k, r];
  (*Para cada valor de soma, 
  procuro a \[UAcute]ltima vizinhan\[CCedilla]a correspondente*)
  
  lastNeighborhoodsWithValue = 
   Last[Cases[neighbourhoodsSums, {__, #[[2]]}]] & /@ 
    neighbourhoodsSums;
  (*Transformo as \[UAcute]ltimas vizinhan\[CCedilla]as em seus equivalentes decimais*)

    symbolSubscripts = 
   FromDigits[#[[1]], k] & /@ lastNeighborhoodsWithValue;
  (*Crio as vari\[AAcute]veis usando os subscritos encontrados*)
  
  Symbol["x" <> ToString[#]] & /@ symbolSubscripts
  ]

OuterTotalisticTemplate[k_Integer: 2, r_Integer: 1] :=
 Module[
  {neighbourhoodsSums, symbolSubscripts, lastNeighborhoodsWithValue},
  (*Calculo a soma de cada vizinhan\[CCedilla]a do espa\[CCedilla]o, 
  e crio uma lista {vizinhan\[CCedilla]a, soma}*)
  
  neighbourhoodsSums = {#, 
      FromDigits[
       ToString[(Plus @@ #) - #[[\[LeftCeiling]Length[#]/
             2\[RightCeiling]]]] <> 
        ToString[#[[\[LeftCeiling]Length[#]/2\[RightCeiling]]]]]} & /@ 
    AllNeighbourhoods[k, r];
  (*Para cada valor de soma, 
  procuro a \[UAcute]ltima vizinhan\[CCedilla]a correspondente*)
  
  lastNeighborhoodsWithValue = 
   Last[Cases[neighbourhoodsSums, {__, #[[2]]}]] & /@ 
    neighbourhoodsSums;
  (*Transformo as \[UAcute]ltimas vizinhan\[CCedilla]as em seus equivalentes decimais*)

    symbolSubscripts = 
   FromDigits[#[[1]], k] & /@ lastNeighborhoodsWithValue;
  (*Crio as vari\[AAcute]veis usando os subscritos encontrados*)
  
  Symbol["x" <> ToString[#]] & /@ symbolSubscripts
  ]


End[];
EndPackage[];
