(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`ExpandTemplate`", "CATemplates`Basic`"];


ExpandTemplate::usage = "Performs a template expansion, i. e. finds all rule tables a template represents in k-ary form. The third optional parameter is  used to perform one expansion at a time, making it feasible to   partition a big expansion in smaller ones.";
ExpandTemplates::usage = "Performs template expansion on a set of templates and applies Union to the resulting set.";


Begin["`Private`"];


RawExpansion[template_List, k_Integer: 2, ithSubstitution_Integer] :=
   Module[
    {templateVariables, transformationRules, substitutions, expansion},
    templateVariables = RuleTemplateVars[template];
    substitutions = Reverse[IntegerDigits[ithSubstitution, k, Length[templateVariables]]];
    transformationRules = MapThread[#1 -> #2 &, {templateVariables, substitutions}];
    (*Aplico as substitui\[CCedilla]\[OTilde]es nas vari\[AAcute]veis.*)
    expansion = template /. transformationRules;
    If[! SubsetQ[Range[0, k - 1], Union[Cases[expansion, x_Integer, 2]]],
      (*Substitui\[CCedilla]\[ATilde]o inv\[AAcute]lida, retorno vazio*)
      {},
      expansion
      ]
    ]

VarConstraintExpansion[expansion_List] :=
  Module[{holds},
    (*Extraio as express\[OTilde]es da forma (x \[Element] {__}) e substituo por Hold[MemberQ[{__}, x]].
    O Hold \[EAcute] necess\[AAcute]rio para que o MemberQ n\[ATilde]o seja avaliado antes da substitui\[CCedilla]\[ATilde]o. *)
    holds = Cases[expansion, (value_ \[Element] set_List -> Hold[MemberQ[set, value]]), Infinity];
    (*Solto o Hold nas express\[OTilde]es pra ver se alguma delas d\[AAcute] false*)   
    holds = ReleaseHold /@ holds;
    (*Se existir alguma express\[ATilde]o dando False, 
    a substitui\[CCedilla]\[ATilde]o n\[ATilde]o \[EAcute] v\[AAcute]lida. 
    O And retorna False caso qualquer uma das express\[OTilde]es seja False, 
    independente do tipo das outras.*)
    If[(And @@ holds) === False,
      {},
      expansion /. (value_ \[Element] set_List -> value)
      ]
    ]

FunctionDefinitionExpansion[expansion_List] :=
    If[MatchQ[#, _Colon], First[#] /. Last[#], #] & /@ expansion;

ExpandTemplate[templateSet_ /; MatchQ[templateSet, {{___} ..}], k_: 2] :=
  Select[Flatten[ExpandTemplate[#, k] & /@ templateSet, 1], Length[#] != 0 &];

ExpandTemplate[template_List /; Not[MatchQ[template, {{___} ..}]], k_Integer: 2] :=
    ExpandTemplate[template, k, {0, (k^Length[RuleTemplateVars[template]]) - 1}];

ExpandTemplate[template_List /; Not[MatchQ[template, {{___} ..}]], k_Integer: 2, {substitutionFrom_, substitutionTo_}] :=
    Select[ExpandTemplate[template, k, #] & /@ Range[substitutionFrom, substitutionTo], Length [#] != 0 &];

ExpandTemplate[template_List /; Not[MatchQ[template, {{___} ..}]], k_Integer: 2, ithSubstitution_Integer] :=
    FunctionDefinitionExpansion[VarConstraintExpansion[RawExpansion[template, k, ithSubstitution]]];




End[];
EndPackage[];
