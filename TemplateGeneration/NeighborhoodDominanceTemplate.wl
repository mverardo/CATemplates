(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`NeighborhoodDominanceTemplate`", "CATemplates`Basic`"];


NDTemplate::usage="Generates a template representative of all the rules that have a given nieghbourhood dominance value for a given space (defined by k and r).";


MaxNDTemplate::usage="Generates the rule with maximum Neighborhood Dominance value for a given space.";


Begin["`Private`"];


NBWeight::usage = 
  "Returns the weight of a given neighborhood for the neighborhood \
dominance calculation";
NBWeight[nb_List] := If[Length[Union[nb]] == 1, 3, 1];

NBWeight::usage = 
  "Returns the weight of a given template var for the neighborhood \
dominance calculation";
NBWeight[templateVar_Symbol, k_Integer: 2, r_Integer: 1] := 
  NBWeight[NeighborhoodFromTemplateVar[templateVar]];

NDTemplate[value_Integer, k_Integer: 2, r_Integer: 1] :=
 Module[
  {baseTemplate, neighbourhoods, commonest, homogeneousNBCount, 
   maxND, subsets, unequalComplements, baseTemplateCommonest, 
   equations, inequations, systems},
  neighbourhoods = AllNeighbourhoods[k, r];
  baseTemplate = OldBaseTemplate[k, r];
  maxND = Plus @@ (NBWeight[#] & /@ neighbourhoods);
  commonest = First[Commonest[#]] & /@ neighbourhoods;
  baseTemplateCommonest = 
   MapThread[{#1, #2} &, {baseTemplate, commonest}];
  (*subsets = Subsets[baseTemplateCommonest,{value}];*)
  (* Aqui, 
  pego todos os subconjuntos de tamanho igual ao value no m\[AAcute]ximo, 
  e seleciono apenas aqueles que tenham a soma dos pesos das \
vizinhan\[CCedilla]as iguais ao valor pedido*)
  
  subsets = 
   Select[Subsets[baseTemplateCommonest, value], 
    Plus @@ (Map[NBWeight@*First, #]) == value &];
  unequalComplements = 
   Complement[baseTemplateCommonest, #] & /@ subsets;
  equations = Apply[Rule, #] & /@ # & /@ subsets;
  inequations = (#[[1]] ->  1 - #[[2]]) & /@ # & /@ unequalComplements;
  systems = MapThread[Join[#1, #2] &, {equations, inequations}];
  baseTemplate /. systems
  ];

MaxNDTemplate[k_Integer: 2, r_Integer: 1] :=
 Module[
  {neighbourhoods},
  neighbourhoods = AllNeighbourhoods[k, r];
  First[Commonest[#]] & /@ neighbourhoods
  ];


End[];
EndPackage[];
