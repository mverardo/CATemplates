(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`ColorBlindTemplate`", "CATemplates`Basic`", "CATemplates`TemplateOperations`TemplateIntersection`"];


ColorBlindTemplate::usage="Generates a template representative of all the color blind rules of a given space (defined by k and r).";


Begin["`Private`"];


FindNBRelationship[permutation_,nb_]:=
  Drop[NestWhileList[ReplaceAll[#, permutation]&,nb,Last[{##}] !=nb&,2],-1]


FindNBRelationships[permutation_, {}] := {};
FindNBRelationships[permutation_, nbs_List]:=
  Module[
    {firstNBRelationships, filteredNBs},
    firstNBRelationships = FindNBRelationship[permutation, First[nbs]];
    filteredNBs = Select[nbs,Not[MemberQ[firstNBRelationships,#]]&];
    Append[FindNBRelationships[permutation,filteredNBs],firstNBRelationships]
  ]


(*Finds an interpolating polynomial that passes through the points given by permutation, and returns a function that applyes the polynomial to a variable*)
FindPermutationPolynomial[permutation_, k_Integer]:=
  Module[
    {points, polynomial},
	points = (List @@ #) & /@ permutation;
    (*Gero o polin\[OHat]mio *)
    polynomial = Together[Expand[InterpolatingPolynomial[points, tvar]]];
    (**)
    Function[var, polynomial /. tvar->var]
  ];


(*Faz com que todas as vari\[AAcute]veis de varSet se transformem em fun\[CCedilla]\[OTilde]es f da primeira. Ex: FindVarRelationships[{x0,x1,x2}] \[Equal] {x0\[Rule]x0, x1\[Rule]f[x0], x2\[Rule]f[f[x0]]} *)
FindVarRelationships[varSet_List, f_Function]:=
  MapIndexed[Rule[#1, Nest[f,First[varSet],#2[[1]]-1]]&,varSet];


ColorBlindTemplate[k_Integer: 2, r_Integer: 1] :=
  With[
    {cbTemplates = ColorBlindTemplate[#, k, r] & /@ PossibleStateReplacements[k]},
    Fold[TemplateIntersection, cbTemplates[[1]], Drop[cbTemplates,1]]
  ];

ColorBlindTemplate[permutation_List, k_Integer: 2, r_Integer: 1] :=
  Module[
    {basetemplate, neighbourhoods, invariantValues, invariantNeighborhoods, invariantReplacementRules, neighbourhoodEquivalences, polyFunction, templateVarEquivalences, replacementRules},
    basetemplate = BaseTemplate[k, r];
    neighbourhoods = Reverse[AllNeighbourhoods[k, r]];
    (*Detecto quais s\[ATilde]o os valores invariantes da permuta\[CCedilla]\[ATilde]o.*)
    invariantValues = Cases[List@@#&/@permutation, {x_,x_} -> x];
    (*Gero os conjuntos de vizinhan\[CCedilla]as relacionadas de acordo com a permuta\[CCedilla]\[ATilde]o. Ex: {{0,0,0},{1,1,1},{2,2,2}}*)
    (*neighbourhoodEquivalences = NestList[ReplaceAll[#, permutation]&,#, k-1] & /@ neighbourhoods*)
    neighbourhoodEquivalences = FindNBRelationships[permutation, neighbourhoods];
    invariantNeighborhoods  = First/@Select[neighbourhoodEquivalences, Length[#] == 1 &];
    invariantReplacementRules = If[Length[invariantValues]==1, 
                                   TemplateVarFromNeighbourhood[#,k] -> First[invariantValues],
                                   TemplateVarFromNeighbourhood[#,k] -> (TemplateVarFromNeighbourhood[#,k] \[Element] invariantValues)
                                ]&/@invariantNeighborhoods;
    (*Removo as que se relacionam com elas mesmas. Ex: {{0,0,0},{0,0,0}} quando a permuta\[CCedilla]\[ATilde]o faz 0\[Rule]0*)
    neighbourhoodEquivalences = Select[neighbourhoodEquivalences, Length[Union[#]] != 1 &];
    (*Transformo todas as vizinhan\[CCedilla]as nas vari\[AAcute]veis de templates *)
    templateVarEquivalences = Map[TemplateVarFromNeighbourhood[#, k] &, neighbourhoodEquivalences, {2}];
    (*Encontro o polin\[OHat]mio gerador da permuta\[CCedilla]\[ATilde]o como uma fun\[CCedilla]\[ATilde]o aplic\[AAcute]vel \[AGrave]s vari\[AAcute]veis.*)
    polyFunction = FindPermutationPolynomial[permutation, k];
    replacementRules = Flatten[FindVarRelationships[#, polyFunction] & /@ templateVarEquivalences, 1];
    replacementRules = Join[invariantReplacementRules, replacementRules];
    (* Aplico as transforma\[CCedilla]\[OTilde]es no template base do espa\[CCedilla]o. *)
    basetemplate /. replacementRules
  ];



End[];
EndPackage[];
