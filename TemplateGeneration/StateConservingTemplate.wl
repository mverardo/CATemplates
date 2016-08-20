(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`BFConservationTemplate`",
  {
    "CATemplates`CA`",
    "CATemplates`CATemplate`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`FilterOutOfRange`",
    "CATemplates`TemplateOperations`Expansion`PostExpansionFn`TemplateMod`"
  }];


StateConservingTemplate::usage="Generates a template representative of all the conservative rules of a given space (defined by k and r).";

ModNStateConservingTemplate::usage="Generates a template representative of all the conservative rules Mod N of a given space (defined by k and r).";


Begin["`Private`"];

BFCondition[nb_, template_, k_Integer, r_Real] :=
    {
      RuleOutputFromNeighbourhood[nb, template, k, r],
      First[nb] +
          Sum[(RuleOutputFromNeighbourhood[Join[Table[0,{i}],Take[nb,{2,2r-i+2}]], template, k, r]
              - RuleOutputFromNeighbourhood[Join[Table[0,{i}],Take[nb,{1,2r-i+1}]], template, k, r]),
            {i, 1, 2r}]};

BFEquations[template_, k_, r_, relevantNBs_]:=
    (Equal @@ #) & /@ (BFCondition[#, template, k, r] & /@ relevantNBs);

BFSolutions[k_Integer: 2, r_Real: 1.0, solveFunction_Function] :=
    Module[{
      basetemplate = BaseTemplateCore[k,r],
      relevantNeighbourhoods = Join[{Table[0, {2 r + 1}]}, Cases[AllNeighbourhoods[k, r], {x_ /; x != 0, ___}]],
      vars, equations, solutions},
      vars = TemplateVarFromNeighbourhood[#,k] & /@ relevantNeighbourhoods;
      equations = BFEquations[basetemplate, k, r, relevantNeighbourhoods];
      solutions = Quiet[Solve[equations, vars]]
    ];

DefaultSolve[] :=
    Function[{equations, vars}, Solve[equations, vars]];

ModularSolve[N_Integer] :=
    Function[{equations, vars}, Solve[equations, vars, Module->N]];

StateConservingTemplate[k_Integer: 2, r_Real: 1.0] :=
    Module[{basetemplate = BaseTemplateCore[k,r], solutions, replacementRules},
      solutions = BFSolutions[k, r, DefaultSolve[]];
      replacementRules = CoreVarsFromConstants[First[solutions]];
      BuildTemplate[k, r, basetemplate /. replacementRules, FilterOutOfRange]
    ];

ModNStateConservingTemplate[N_Integer: 2, k_Integer: 2, r_Real: 1.0] :=
    Module[{basetemplate = BaseTemplateCore[k,r], solutions, replacementRules},
      solutions = BFSolutions[k, r, ModularSolve[N]];
      replacementRules = CoreVarsFromConstants[First[solutions]];
      BuildTemplate[k, r, basetemplate /. replacementRules, TemplateMod, N]
    ];

End[];
EndPackage[];
