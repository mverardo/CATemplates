(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`TemplateIntersection`", "CATemplates`Basic`"];


ClarAll[TemplateIntersection];


TemplateIntersection::usage="Returns a template that represents the intersection between two other templates."


Begin["`Private`"];


RawIntersection[template1_List, template2_List] :=
  Module[
    {templates, templateVars, noSetTemplates, equations, intersectionSolution, semiIntersectedTemplates, elementExpressions, elementEquations, elementSolution, solutionElementExpressions, solutionElementSubstitutions},
    templates = {template1, template2};
    templateVars = Union[Flatten[RuleTemplateVars[#] & /@ templates, 1]];
    noSetTemplates = templates /. x_ \[Element] y_ -> x;
    equations = Equal @@ # & /@ Transpose[noSetTemplates];
    intersectionSolution = Quiet @ Solve[equations, templateVars];
    (*Removendo os resultados racionais.*)
	intersectionSolution = Select[intersectionSolution,( And @@ (Not[MatchQ[#,Rule[_,_Rational]]]&/@#))&];
    If[intersectionSolution === {},
      {},
      (*Aplico as substitui\[CCedilla]\[OTilde]es obtidas no sistema anterior em todos os templates para deix\[AAcute]-los "equivalentes" em termos de suas vari\[AAcute]veis.*)
      semiIntersectedTemplates = Flatten[templates /. intersectionSolution, 1];
      (*Retorno a uni\[ATilde]o dos templates encontrados. 
      Se n\[ATilde]o existirem express\[OTilde]es especiais (fun\[CCedilla]\[OTilde]es PaP e restri\[CCedilla]\[ATilde]o de dom\[IAcute]nio, a intersec\[CCedilla]\[ATilde]o termina aqui.)*)
      With[{union = Union[semiIntersectedTemplates]}, 
	    If[Length@union == 1,
          First[union], 
          union
        ]
      ]
    ]
  ];


VarConstraintIntersection[template1_, template2_]:=
  Module[
  {templates, elementExpressions, equations, solution, solutionElementExpressions, solutionElementSubstitutions},
  templates = {template1, template2};
  elementExpressions = Cases[templates, x_ \[Element] set_ ,Infinity];
  equations = Function[elementExpression, Apply[Or,elementExpression[[1]] == #&/@ elementExpression[[2]]]] /@ elementExpressions;
  solution = Quiet @ Solve[equations];
  (* Se o solve n\[ATilde]o retornar solu\[CCedilla]\[ATilde]o, os templates n\[ATilde]o se intersectam em lugar nenhum. Importante: express\[OTilde]es element inv\[ATilde]lidas, como 0 \[Element] {1,2} far\[ATilde]o o Solve retornar um conjunto vazio. *)
  If[solution === {},
    {},
    solutionElementExpressions = #[[1,1]] \[Element] Union[Last/@#] & /@ Transpose[solution];
    solutionElementSubstitutions = If[Length[#[[2]]] == 1, #[[1]] -> #[[2,1]],#[[1]] -> #]& /@ solutionElementExpressions;
    (*Removo as express\[OTilde]es element antigas. Importante notar que no processo, qualquer express\[ATilde]o element que levasse um inteiro para um set some.*)
    templates = templates /. x_ \[Element] _List -> x;
    templates = templates /.solutionElementSubstitutions;
    First[templates]
  ]
 ]


(* The intersection between a template and an equivalent template is the template itself. *)
TemplateIntersection[template_, template_] := template;


TemplateIntersection[x_ /; MatchQ[x, {{__} ..}], y_ /; Not[MatchQ[y, {{__} ..}]]] :=
  TemplateIntersection[x, {y}];

TemplateIntersection[x_ /; Not[MatchQ[x, {{__} ..}]], y_ /; MatchQ[y, {{__} ..}]] :=
  TemplateIntersection[{x}, y];


(* The intersection between two sets of templates is given by the outer product of the intersection over the sets. *)
TemplateIntersection[x_ /; MatchQ[x, {{__} ..}], y_ /; MatchQ[y, {{__} ..}]] :=
  With[
    {intersectionResult = Select[TemplateIntersection[#[[1]], #[[2]]] & /@ Flatten[Outer[{#1, #2} &, x, y, 1], 1], (# != {} && ValidTemplateQ[#]) &]},
    If[Length[intersectionResult]==1,
      First[intersectionResult], 
      intersectionResult
    ]
  ];
 


TemplateIntersection[template1_, template2_] := 
  Module[
	{templates, varConstrainedTemplates, functionDefinitionTemplates, rawTemplates, rawIntersection},
	templates = {template1, template2};
	functionDefinitionTemplates = templates /. Element[x_,set_] -> x;
	varConstrainedTemplates = templates /. Colon[x_, f_] -> x;
	rawTemplates = templates /. Element[x_,set_] -> x /.Colon[x_, f_] -> x;
    
    (*Otimizando para o caso mais comum: Se n\[ATilde]o existir sintaxe especial, fa\[CCedilla]o a intersec\[CCedilla]\[ATilde]o comum.*)
    If[functionDefinitionTemplates === rawTemplates && rawTemplates === varConstrainedTemplates,
      Return[RawIntersection[rawTemplates[[1]], rawTemplates[[2]]]]
    ];
	(*Se n\[ATilde]o existirem defini\[CCedilla]\[OTilde]es de fun\[CCedilla]\[ATilde]o ponto a ponto, fa\[CCedilla]o a intersec\[CCedilla]\[ATilde]o normal e depois trato as restri\[CCedilla]\[OTilde]es de vari\[AAcute]veis.*)
    If[functionDefinitionTemplates === rawTemplates,
      rawIntersection = RawIntersection[varConstrainedTemplates[[1]], varConstrainedTemplates[[2]]];
      (*Se o resultado da rawIntersection n\[ATilde]o est\[AAcute] unificado, preciso aplicar VarConstraintIntersection*)
      If[MatchQ[rawIntersection,{{__},{__}}],
        Return[VarConstraintIntersection[rawIntersection[[1]],rawIntersection[[2]]]]
      ];
	  Return[rawIntersection];
    ];

	(* Se n\[ATilde]o existirem restri\[CCedilla]\[OTilde]es de vari\[AAcute]veis, fa\[CCedilla]o a intersec\[CCedilla]\[ATilde]o de fun\[CCedilla]\[OTilde]es ponto a ponto e depois a normal. *)

	Throw["Not implemented yet"]
  ]


End[];
EndPackage[];
