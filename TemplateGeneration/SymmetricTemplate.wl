(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`SymmetricTemplate`", "CATemplates`Basic`"];


SymmetricTemplate::usage="Returns a set of templates representative of all the rules in a space given by k and r that share a symmetryValue according to transform.";


Begin["`Private`"];



SymmetricTemplate[transform_, symmetryValue_Integer, k_Integer: 2, 
  r_Integer: 1, templateQuantity_: All] :=
  Module[
    {neighbourhoods, neighbourhoodEquivalences, templateEquivalences, transformedEquivalences, equivalenceSubsets, unequalComplements, equations, inequations, systems, solutions,invariantNeighborhoodQuantity},
    neighbourhoods = AllNeighbourhoods[k, r];
    neighbourhoodEquivalences = {#, transform[#]} & /@ neighbourhoods;
    (*Descobrindo quantas vizinhan\[CCedilla]as s\[ATilde]o invariantes*)
    invariantNeighborhoodQuantity = Length[Select[neighbourhoodEquivalences, #[[1]] == #[[2]] &]];
    (*Removo as vizinhan\[CCedilla]as invariantes quanto \[AGrave] transforma\[CCedilla]\[ATilde]o*)
    neighbourhoodEquivalences = Select[neighbourhoodEquivalences, #[[1]] != #[[2]] &];
    (* Se a quantidade de vizinhan\[CCedilla]as invariantes for maior do que o valor de simetria solicitado... *)
    If[symmetryValue < invariantNeighborhoodQuantity, 
      Return[{}]
    ];
    (*Removendo as duplas iguais que aparecem em ordens diferentes({{1,0,0}{0,0,1}} \[EAcute] equivalente a {{0,0,1},{1,0,0}}, por exemplo.)*)
    neighbourhoodEquivalences = Union[Sort /@ neighbourhoodEquivalences];
    templateEquivalences = TemplateVarFromNeighbourhood /@ # & /@ neighbourhoodEquivalences;
    (*Como a transforma\[CCedilla]\[ATilde]o LR n\[ATilde]o altera o valor do output, esse step n\[ATilde]o \[EAcute] necess\[AAcute]rio*)
    transformedEquivalences = {#[[2]], transform[#[[1]]]} & /@ templateEquivalences;
    (*Monto os subsets usando apenas metade do valor de simetria pedido, j\[AAcute] que metade das vizinhan\[CCedilla]as foram removidas pelo Union*)
    equivalenceSubsets = Subsets[transformedEquivalences, {(symmetryValue - invariantNeighborhoodQuantity)/2}, templateQuantity];
    unequalComplements = Complement[transformedEquivalences, #] & /@ equivalenceSubsets;
    equations = Apply[Equal, #] & /@ # & /@ equivalenceSubsets;
    inequations = (#[[1]] == 1 - #[[2]]) & /@ # & /@ unequalComplements;
    systems = MapThread[Join[#1, #2] &, {equations, inequations}];
    systems = systems /. x__ == y__ -> Rule[x, y] ;
    BaseTemplate[k, r] /. systems
  ]


End[];
EndPackage[];
