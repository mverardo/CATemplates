(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`ColorBlindTemplate`",
  {
    "CATemplates`CA`",
    "CATemplates`CATemplate`",
    "CATemplates`TemplateOperations`Intersection`TemplateIntersection`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`FilterNotAllowed`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`ModK`"
  }];


PossiblePermutations::usage="Generates a list of all color permutations possible for a given k";
ColorBlindTemplate::usage="Generates a template representative of all the color blind rules of a given space (defined by k and r).";


Begin["`Private`"];

PossiblePermutations[k_Integer: 2] :=
    With[
      {permuts = Permutations[Range[0, k - 1]]},
      MapThread[Thread[#1 -> #2] &, {Table[First@permuts, {Length[permuts] - 1}], Rest@permuts}]
    ];

EquivalenceClassByPermutation::usage="Finds the equivalence class of a neighbourhood given by a permutation";
EquivalenceClassByPermutation[nb_, permutation_] :=
    Drop[NestWhileList[ReplaceAll[#, permutation]&,nb,Last[{##}] !=nb&,2],-1];


PartitionByPermutation::usage="Partitions a set of neighbourhoods according to the equivalence classes given by a permutation."
PartitionByPermutation[{}, permutation_] := {};
PartitionByPermutation[nbs_List, permutation_]:=
    Module[
      {firstNBRelationships, filteredNBs},
      firstNBRelationships = EquivalenceClassByPermutation[First[nbs], permutation];
      filteredNBs = Select[nbs, Not[MemberQ[firstNBRelationships, #]] &];
      Append[PartitionByPermutation[filteredNBs, permutation], firstNBRelationships]
    ];


(*Finds an interpolating polynomial that passes through the points given by permutation, and returns a function that applyes the polynomial to a variable*)
FindPermutationPolynomial[permutation_, k_Integer]:=
    Module[
      {points, polynomial},
    	points = (List @@ #) & /@ permutation;
      polynomial = Together[Expand[InterpolatingPolynomial[points, tvar]]];
      Function[var, polynomial /. tvar->var]
    ];


(*Faz com que todas as vari\[AAcute]veis de varSet se transformem em fun\[CCedilla]\[OTilde]es f da primeira. Ex: FindVarRelationships[{x0,x1,x2}] \[Equal] {x0\[Rule]x0, x1\[Rule]f[x0], x2\[Rule]f[f[x0]]} *)
FindVarRelationships[varSet_List, f_Function]:=
    MapIndexed[Rule[#1, Nest[f,First[varSet],#2[[1]]-1]]&,varSet];

InvariantReplacementRules[nbEqClasses_, permutation_, k_] :=
    With[{
      invariantValues = Cases[(List @@ #) & /@ permutation, {x_,x_} -> x],
      invariantNeighborhoods = First/@Select[nbEqClasses, Length[#] == 1 &]},
      If[Length[invariantValues]==1,
        TemplateVarFromNeighbourhood[#,k] -> First[invariantValues],
        TemplateVarFromNeighbourhood[#,k] -> (TemplateVarFromNeighbourhood[#,k] \[Element] invariantValues)
      ]& /@ invariantNeighborhoods
    ];

VariantReplacementRules[nbEqClasses_, permutation_, k_] :=
    With[{
      templateVarEquivalences = Map[TemplateVarFromNeighbourhood[#, k] &, nbEqClasses, {2}],
      polyFunction = FindPermutationPolynomial[permutation, k]},
      Flatten[FindVarRelationships[#, polyFunction] & /@ templateVarEquivalences, 1]
    ];

ColorBlindTemplate[k_Integer: 2, r_Real: 1.0] :=
    With[
      {cbTemplates = ColorBlindTemplate[#, k, r] & /@ PossiblePermutations[k]},
      Fold[TemplateIntersection[#2, #1]&, cbTemplates[[1]], Rest[cbTemplates]]
    ];

ColorBlindTemplate[permutation_List, k_Integer: 2, r_Real: 1.0] :=
    Module[{neighbourhoods, nbEqClasses, replacementRules},
      neighbourhoods = Reverse[AllNeighbourhoods[k, r]];
      nbEqClasses = PartitionByPermutation[neighbourhoods, permutation];
      replacementRules = Join[InvariantReplacementRules[nbEqClasses, permutation, k], VariantReplacementRules[nbEqClasses, permutation, k]];
      BuildTemplate[k, r, BaseTemplateCore[k, r] /. replacementRules, {FilterNotAllowed, ModK}]
    ];

End[];
EndPackage[];
