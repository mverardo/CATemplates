(* ::Package:: *)

BeginPackage["CATemplates`TemplateGeneration`ModNStateConservingTemplate`", "CATemplates`Basic`"];


 ModNStateConservingTemplate::usage = "Bla";


Begin["`Private`"];


BFEquations[template_, k_, r_, relevantNBs_]:= (Equal @@ #) & /@ ({RuleOutputFromNeighbourhood[#, template, k, r],
        First[#] + \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(2  r\)]\((RuleOutputFromNeighbourhood[Join[Table[0, {i}], Take[#, {2, 2  r - i + 2}]], \ template, \ k, \ r]\n\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \  - \ RuleOutputFromNeighbourhood[Join[Table[0, {i}], Take[#, {1, 2  r - i + 1}]], \ template, \ k, \ r])\)\)} & /@ 
        relevantNBs);


 ModNStateConservingTemplate[N_Integer, k_Integer:2, r_:1] :=
   Module[{basetemplate,vars,relevantNeighbourhoods,equations, solutions, replacementRules},
      basetemplate = BaseTemplate[k,r];
      relevantNeighbourhoods = Join[{Table[0, {2 r + 1}]}, Cases[AllNeighbourhoods[k, r], {x_ /; x != 0, ___}]];
      vars = Reverse[RuleTemplateVars[basetemplate]];
      equations = BFEquations[basetemplate, k, r, relevantNeighbourhoods];
      solutions = Quiet[Solve[equations, vars, Modulus->N]];
      replacementRules = ConstantsToVariables[First[solutions]];
      basetemplate /. replacementRules
   ];


End[];
EndPackage[];
