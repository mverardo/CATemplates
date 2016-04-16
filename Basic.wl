(* ::Package:: *)

BeginPackage["CATemplates`Basic`"];


MinimizeSets::usage = "Minimize a subset boolean expression";
Partial::usage = "Partial[f_, args__] := partially applies arguments args to function f.";

PrintTestResults::usage = "PrintTestResults[testReport_] := Prints the results of a testReport in a terminal friendly manner";

SubstitutionRange::usage = "SubstitutionRange[template_Association] := Gives a range from 0 to the maximum possible substitution the template could have";

OldBaseTemplate::usage = "Gives the base template for a radius r k-ary rule.";
ValidTemplateQ::usage = "Determines if a template has avalid form.";
RuleTemplateVars::usage = "Extracts the variable names used in a rule template and returns them in a list. The names are given in lexicographical order; for instance, RuleTemplateVars[MaxSymmTemplate[{BWLR, BW, LR}, 2, 1]] returns {x2, x1, x0}. If the template is in the k-ary form, the function returns {}.";
TakeNeighbourhoods::usage = "Returns the n first neighborhoods from a given space.";
TemplateFromNeighbourhoods::usage = "Builds a template given a list of neighbourhoods. Converts the neighbourhoods to symbols in the form xN, where N is the decimal conversion of the k-ary neighbourhood.";
TemplateVarFromNeighbourhood::usage = "Returns the template symbol orresponding to a given neighbourhood";
NeighborhoodFromTemplateVar::usage = "Returns the neighbourhood corresponding to a given template symbol";
BWTransform::usage = "Performs the Black-White transform in a rule table, transition, neighbourhood or transition output.";
LRTransform::usage = "Performs the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
BWLRTransform::usage = "Performs the Black-White transform followed by the Left-Right transform in a rule table, transition, neighbourhood or transition output.";
LRBWTransform::usage = "Performs the Left-Right transform followed by the Black-White transform in a rule table, transition, neighbourhood or transition output.";
AllNeighbourhoods::usage = "Creates all possible  k^(2r+1)neighbourhoods.";
KAryFromRuleTable::usage = "Auxiliary function that converts a rule table to its k-ary representation.";
RuleTableFromKAry::usage= "Auxiliary function that converts k-ary rule table to its classical representation.";
RuleTable::usage = "Creates the rule table of rnum, under Wolfram\.b4s lexicographic order. ";
RuleOutputFromNeighbourhood::usage="Yields the output bit of a given neighbourhood from rule rnum. The neighbourhood may be given as the k-ary sequence that defines it, or as the decimal number it represents (e.g, decimal 6 for neighbourhood {0, 1, 1, 0}, etc).";

ExceptionTemplates::usage= "ExceptionTemplates[t_List, k_Integer:2, r_Integer:2] generate all the templates with variable assignments that make the template t with k colors and r range invalid.";


PossibleStateReplacements::usage="Retorna todas as permuta\[CCedilla]\[OTilde]es poss\[IAcute]veis de estados de acordo com k.";


RawTemplate::usage="RawTemplate[t_List]: Receives a template t, and drops any special sintax construct from it. Currently, it removes expressions of the form x \[Element] {__}.";


ImprisonmentExpressions::usage="ImprisonmentExpressions[t_List]: Receives a template t, and returns all of the expressions of the form x \[Element] {__}.";


ValueRestrictions::usage = "ValueRestrictions[imprisonmentExpression_]: Returns an expression that represents the value restricions dictated by an ImprisonmentExpression. Example: ValueRestrictions[x1 \[Element] {0,1}] -> x1 == 0 || x1 == 1";


FreeVariableQ::usage = "FreeVariableQ[expression_]: Receives an expression, and returns true if the expression is a free variable and false otherwise.";


CorrespondsToNeighborhoodQ::usage = "CorrespondsToNeighborhoodQ[freeVariable_Symbol, nbIndex_Integer]: Receives a free variable expression and a neighborhood index, and returns true if the variable's index corresponds to the received nb index.";


PreservesIndexVariableDualityQ::usage = "PreservesIndexVariableDualityQ[template_]: Receives a template and returns true if the template preserver the index-variable diality.";


ConstantsToVariables::usage = "ConstantsToVariables[replacementRules_]: Receives a list of replacement rules, and converts any symbol of the type C[i_Integer] into its corresponding template variable, preserving the index-variable duality."

Begin["`Private`"];

SetAttributes[Partial, HoldAll];
Partial[f_, as__] := Function[Null, f[as, ##], HoldAll];

SubstitutionRange[template_Association] :=
    Range[0, (template[["k"]]^Length[RuleTemplateVars[template]])-1];

OldBaseTemplate[k_Integer: 2, r_: 1] :=
  Symbol["x" <> ToString[#]] & /@ Range[(\!\(\*SuperscriptBox[\(k\), \(\[LeftCeiling]r*2\[RightCeiling] + 1\)]\)) - 1, 0, -1];

ValidTemplateQ[template_] :=
  And @@ (MatchQ[#, (_Symbol | _Integer | _Plus | _Times | _ \[Element] {__})] & /@ template);

RuleTemplateVars[ruletemplate_Association] :=
    RuleTemplateVars[ruletemplate[["rawList"]]];

RuleTemplateVars[ruletemplate_] :=
  SortBy[Union[Cases[ruletemplate, _Symbol, Infinity]], FromDigits[StringDrop[SymbolName[#],1]]&]

TakeNeighbourhoods[n_Integer, k_Integer: 2, r_Integer: 1] :=
  With[{neighbourhoodCount = k^(2 r + 1)},
    If[n <= neighbourhoodCount,
      IntegerDigits[#, k, \[LeftFloor]2 r + 1\[RightFloor]] & /@ Range[neighbourhoodCount - 1, (neighbourhoodCount - n), -1],
      Throw["n can't be bigger than k^(2r+1)"]
    ]
  ]

TemplateFromNeighbourhoods[neighbourhoods_List] :=
 Symbol["x" <> ToString[FromDigits[#, 2]]] & /@ neighbourhoods;

TemplateVarFromNeighbourhood[neighbourhood_List, k_Integer: 2] :=
  Symbol["x" <> ToString@FromDigits[neighbourhood, k]];

NeighborhoodFromTemplateVar[templateVar_Symbol, k_Integer: 2, r_Integer: 1] :=
 IntegerDigits[FromDigits[StringDrop[ToString@templateVar, 1]], k, 2*r + 1];

BWTransform[parameter_] :=
  (*Se o parametro for a rule table inteira*)
  If[MatchQ[parameter, {{x_List, y_}, __}],
    (*Mapeio a pr\[OAcute]pria transforma\[CCedilla]\[ATilde]o em cada transi\[CCedilla]\[ATilde]o, fa\[CCedilla]o um Sort pela vizinhan\[CCedilla]a e inverto a ordem*)
    Reverse[SortBy[BWTransform /@ parameter, First]],
    (*Faz a transforma\[CCedilla]\[ATilde]o em vizinhan\[CCedilla]as, outputs, ou transi\[CCedilla]\[OTilde]es no formato {{vizinhan\[CCedilla]a}, output}*)
    1 - parameter
  ]

LRTransform[parameter_] :=
 (*Se o parametro for a rule table inteira*)
  If[MatchQ[parameter, {{x_List, y_}, __}],
    (*Mapeio a pr\[OAcute]pria transforma\[CCedilla]\[ATilde]o em cada transi\[CCedilla]\[ATilde]o, fa\[CCedilla]o um Sort pela vizinhan\[CCedilla]a e inverto a ordem*)
    Reverse[SortBy[LRTransform /@ parameter, First]],
    (*Se o parametro for um par {{vizinhan\[CCedilla]a}, output}*)
    If[MatchQ[parameter, {x_List, y_}],
      {Reverse [parameter[[1]]], parameter[[2]]},
      (*Se o parametro for apenas a vizinhan\[CCedilla]a*)
      If[MatchQ[parameter, x_List],
        Reverse[parameter],
        (*Se for apenas o output*)
        parameter
      ]
    ]
  ]

BWLRTransform[parameter_] :=
  LRTransform[BWTransform[parameter]]

LRBWTransform[parameter_] :=
  BWTransform[LRTransform[parameter]]

AllNeighbourhoods[k_Integer : 2, r_ : 1] :=
  Tuples[Range[k - 1, 0, -1], \[LeftFloor]2 r + 1\[RightFloor]];

RuleTable[rnum_Integer, k_Integer: 2, r_: 1] :=
  RuleTableFromKAry[PadLeft[IntegerDigits[rnum, k], \!\(\*SuperscriptBox[\(k\), \(\[LeftCeiling]2  r\[RightCeiling] + 1\)]\)], k, r];

KAryFromRuleTable[ruleTable_] :=
  #[[2]] & /@ ruleTable;

RuleTableFromKAry[kAryRuleTable_, k_Integer: 2, r_: 1] :=
  MapThread[List[#1, #2] &,
    {Tuples[Range[k - 1, 0, -1], \[LeftFloor]2 r + 1\[RightFloor]],
    kAryRuleTable}];

RuleOutputFromNeighbourhood[neighbourhood_List, rnum_Integer, k_Integer: 2, r_: 1] :=
  RuleOutputFromNeighbourhood[FromDigits[neighbourhood, k], KAryFromRuleTable[RuleTable[rnum, k, r]], k, r];

RuleOutputFromNeighbourhood[neighbourhood_List, kAryRuleTable_List, k_Integer: 2, r_: 1] :=
  RuleOutputFromNeighbourhood[FromDigits[neighbourhood, k], kAryRuleTable, k, r];

RuleOutputFromNeighbourhood[neighbourhoodindex_Integer, rnum_Integer, k_Integer: 2, r_: 1] :=
  RuleOutputFromNeighbourhood[neighbourhoodindex, KAryFromRuleTable[RuleTable[rnum, k, r]], k, r];

RuleOutputFromNeighbourhood[neighbourhoodindex_Integer, kAryRuleTable_List, k_Integer: 2, r_: 1] :=
  Extract[kAryRuleTable, {\!\(\*SuperscriptBox[\(k\), \(\[LeftFloor]2  r + 1\[RightFloor]\)]\) - neighbourhoodindex}];

PossibleStateReplacements[k_Integer: 2] :=
  With[
    {permuts = Permutations[Range[0, k - 1]]},
    MapThread[Thread[#1 -> #2] &, {Table[First@permuts, {Length[permuts] - 1}], Rest@permuts}]
  ];


RawTemplate[template_]:= template /. Element[x_,set_] -> x;


ImprisonmentExpressions[template_List]:= Cases[template, x_ \[Element] set_ ,Infinity]


ValueRestrictions[imprisonmentExpression_]:=
 Apply[Or,imprisonmentExpression[[1]] == #&/@ imprisonmentExpression[[2]]];

RemoveContainedSubsSets[allSubSet_List, subSet_List] :=
 Module[{listToRemove, result},
  listToRemove = ContainsAll[#, subSet] && subSet =!= # & /@ allSubSet;
  result = MapThread[If[! #2, #1, ## &[]] &, {allSubSet, listToRemove}]
]

SubSetsToBooleanExpression[subSets_List] :=
    Map[And @@ # &,
      (
        Apply[
          Or,
          Map[
            If[#[[2]] === 0,\[Not] #[[1]],#[[1]]] &,
            subSets,
            {2}
          ]
        ]
      )
    ];

BooleanExpressionToSubSetsV2[minimizeBooleanExpression_] := Module[{RootExpressionToSubSet, ExpressionToSubSet, LeafExpressionToSubSet},
  RootExpressionToSubSet[expr_ /; Head[expr] === Or] := Map[ExpressionToSubSet, expr, {1}] /. And -> List /. Or -> List;
  RootExpressionToSubSet[expr_] := {ExpressionToSubSet[expr]} /. And -> List /. Or -> List;

  ExpressionToSubSet[expr_ /; Head[expr] === And] := Map[LeafExpressionToSubSet, expr];
  ExpressionToSubSet[expr_] := {LeafExpressionToSubSet[expr]};

  LeafExpressionToSubSet[expr_ /; Head[expr] === Symbol] := expr -> 1;
  LeafExpressionToSubSet[expr_ /; Head[expr] === Not] := expr[[1]] -> 0;

  result = RootExpressionToSubSet[minimizeBooleanExpression]
];

MinimizeSets[subSets_List] :=
    Module[{result, booleanExpression, minimizeBooleanExpression},
      result = subSets;
      If[Length[result] === 1, result,
        booleanExpression = SubSetsToBooleanExpression[result];
        minimizeBooleanExpression = BooleanMinimize[booleanExpression];
        result = BooleanExpressionToSubSets[minimizeBooleanExpression]
      ]
    ];

InvalidSubSets[intemplate_, k_Integer:2] :=
    Map[#[[1]] &,
      Cases[
        Map[{#[[2]], #[[1]] /. #[[2]]} &,
          Flatten[
            Map[Outer[List, {#[[1]]}, #[[2]], 1] &,
              Map[{
                #[[2]],
                Map[
                  MapThread[#1 -> #2 &, {#[[1]], #[[2]]}] &,
                  #[[1]]
                ]
              } &,
                Map[
                  {First@Outer[List, {#[[1]]}, #[[2]], 1], #[[3]]} &,
                  Map[
                    {#[[1]], Tuples[Range[0, k - 1], Length[#[[1]]]], #[[2]]} &,
                    Map[{RuleTemplateVars[{#}], #} &, Select[intemplate, (Depth[#] > 1) &]
                    ]
                  ]
                ]
              ]
            ]
            , 2
          ]
        ],
        {_, x_ /; \[Not] MemberQ[Range[0, k - 1], x]}
      ]
    ];

ExceptionTemplates[intemplate_] :=
    ExceptionTemplates[intemplate, 2, 1];

ExceptionTemplates[intemplate_, k_ /; k === 2, r_Integer : 1] :=
    Module[{invalidSubsSets, filteredInvalidSubsSets, result = {}},
      invalidSubsSets = Union[
        SortBy[InvalidSubSets[intemplate, k], Total]
      ];
      If[invalidSubsSets =!= {},
        filteredInvalidSubsSets = MinimizeSets[invalidSubsSets];
        result = (OldBaseTemplate[k, r] /. #) & /@ filteredInvalidSubsSets;
      ];
      result
    ];

ExceptionTemplates[intemplate_, k_Integer:2, r_Integer:1] :=
    MapThread[If[#2=== _,#1,#2]&,{OldBaseTemplate[k,r],#}]&/@Union[(If[NumberQ[#],#,_]&/@#)&/@((OldBaseTemplate[k,r]/.#[[1]])&/@Cases[{#[[2]],#[[1]]/.#[[2]]}&/@Flatten[Outer[List,{#[[1]]},#[[2]],1]&/@({#[[2]],MapThread[#1->#2&,{#[[1]],#[[2]]}]&/@#[[1]]}&/@({First@Outer[List,{#[[1]]},#[[2]],1],#[[3]]}&/@({#[[1]],Tuples[Range[0,k-1],Length[#[[1]]]],#[[2]]}&/@({RuleTemplateVars[{#}],#}&/@Select[intemplate,(Depth[#]>1)&])))),2],{_,x_/;\[Not]MemberQ[Range[0,k-1],x]}])]


FreeVariableQ[expression_] := MatchQ[expression, _Symbol];


CorrespondsToNeighborhoodQ[symbol_, nbIndex_] :=
  (FromDigits[StringDrop[SymbolName[symbol], 1]] === nbIndex);


PreservesIndexVariableDualityQ[template_] :=
  And @@ (MapIndexed[(!FreeVariableQ[#1]) || (CorrespondsToNeighborhoodQ[#1, First[#2] - 1]) &, Reverse[template]]);


ConstantsToVariables[replacementRules_List] :=
  Module[{freeVariableReplacementRules},
	freeVariableReplacementRules = Reverse /@ Select[Sort[replacementRules], MatchQ[#,Rule[_Symbol, C[_]]]&];
	replacementRules /. freeVariableReplacementRules
  ]


PrintTestResults[testReport_] :=
    Module[{},
      Print["Suceeded: " <> ToString[testReport["TestsSucceededCount"]]];
      If[testReport["TestsFailedCount"] > 0,
        Print["Failed: " <> ToString[testReport["TestsFailedCount"]] <> "; Indices:" <> ToString[testReport["TestsFailedIndices"]]];
      ];
    ];

End[];
EndPackage[];
