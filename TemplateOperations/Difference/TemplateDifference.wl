(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`TemplateDifference`",
  {
    "CATemplates`Basic`", 
    "CATemplates`TemplateOperations`Intersection`Common`",
    "CATemplates`TemplateOperations`Intersection`TemplateIntersection`"}];


TemplateDifference::usage= "bl1";
DualEquationSystem::usage= "bl2";
DifferenceReplacementRules::usage= "bl3";

Begin["`Private`"];

TemplateDifference[template1_List, template2_List, radius_: 1] :=
 Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
  templateIntersection = TemplateIntersection[template1, template2]//Flatten;
  If[!ValidTemplateQ[templateIntersection] || templateIntersection === {}, template1,
  replacementRules = DifferenceReplacementRules[template1, templateIntersection];
  replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];(*ATEN\[CapitalCCedilla]\[CapitalATilde]O: Aqui remove todas as regras que contenham n\[UAcute]meros racionais*)


  If[replacementRulesFinal == {}, {},
   templateDifferenceP1 = template1 /. replacementRulesFinal;(*Apply*)
   exceptionTemplates = ExceptionTemplates[templateIntersection, 2, radius];
   templateDifferenceP2 = TemplateIntersection[template1, #] & /@ exceptionTemplates;
   templateDifference = Join[templateDifferenceP1, templateDifferenceP2]
   (*cleanTemplateDifference = DeleteCases[Union[templateDifference], template1]*)
   ]
  ]
 ]

DualEquationSystem[template1_List, template2_List] :=
 Module[{notTemplate2, equationSystem,cleanEquationSystem, dualEquationSystem,dualEquationSystemResult},
  (*notTemplate2 = 1 - template2; (*Change 0 to 1 and 1 to 0*);*)
  equationSystem = EquationSystem[template1,template2];(*Make a equation System*)
  cleanEquationSystem = Select[equationSystem,!TrueQ[#]&];(*Remove tautologies*)
  dualEquationSystem = Map[Part[#,1]==1-(Part[#,2])&,cleanEquationSystem];(*Apply Not to all variables*)
  dualEquationSystemResult = Or @@ (# & /@ dualEquationSystem) (*Change boolean operator AND to OR*)
  ]

DifferenceReplacementRules[template1_, template2_, k_Integer: 0] :=
  Module[{
  templateVars = SortBy[Union[Flatten[RuleTemplateVars[#] & /@ {template1,template2}, 1]], 
  FromDigits[StringDrop[SymbolName[#], 1]] &],
  dualEquationSystem
  },
  dualEquationSystem = DualEquationSystem[template1, template2];
  If[dualEquationSystem =!= False,
	If[k === 0,
		Quiet[Solve[DualEquationSystem[template1, template2], templateVars]]//Union,
		Quiet[Solve[DualEquationSystem[template1, template2], Reverse[templateVars], Modulus -> k]]//Union
	],{}
  ]
];

End[];
EndPackage[];
