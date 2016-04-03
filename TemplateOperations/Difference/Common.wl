(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`Difference`Common`", "CATemplates`Basic`"];


DualEquationSystem::usage = "bl2";
DifferenceReplacementRules::usage = "
DifferenceReplacementRules[template1_List, template2_List]: Takes two templates template1 and template2, and returns the replacement rules that could be applied to template1 or template2 in order to find the diference between then. Returns {} if there is no possible diference template or if they are both templates are the same.
DifferenceReplacementRules[template1_List, template2_List, modulus_Integer]: Takes two templates template1 and template2, and returns the replacement rules that could be applied to template1 or template2 in order to find the diference between then. Returns {} if there is no possible diference template or if they are both templates are the same. Assumes both templates are modular.
";


Begin["`Private`"];

EquationSystem[template1_List,template2_List]:=
    Equal @@ # & /@ Transpose[{template1, template2}];

DualEquationSystem[template1_List, template2_List] :=
    Module[{notTemplate2, equationSystem, cleanEquationSystem, dualEquationSystem, dualEquationSystemResult},
      equationSystem = EquationSystem[template1, template2];(*Make a equation System*)
      cleanEquationSystem = Select[equationSystem, !TrueQ[#]&];(*Remove tautologies*)
      dualEquationSystem = Map[Part[#, 1] == 1 - (Part[#, 2])&, cleanEquationSystem];(*Apply Not to all variables*)
      dualEquationSystemResult = Or @@ (# & /@ dualEquationSystem) (*Change boolean operator AND to OR*)
    ];

DifferenceReplacementRules[template1_List, template2_List, modulus_Integer : 0] :=
    Module[{
      templateVars = SortBy[Union[Flatten[{RuleTemplateVars[template1], RuleTemplateVars[template2]}, 1]],
        FromDigits[StringDrop[SymbolName[#], 1]] &],
      dualEquationSystem
    },
      dualEquationSystem = DualEquationSystem[template1, template2];
      If[dualEquationSystem =!= False,
        If[modulus === 0,
          Union[Quiet[Solve[DualEquationSystem[template1, template2], templateVars]]],
          Union[Quiet[Solve[DualEquationSystem[template1, template2], Reverse[templateVars], Modulus -> modulus]]]
        ], {}
      ]
    ];

End[];
EndPackage[];
