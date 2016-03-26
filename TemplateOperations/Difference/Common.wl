(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`Difference`Common`", "CATemplates`Basic`"];


DualEquationSystem::usage = "bl2";
DifferenceReplacementRules::usage = "
ReplacementRules[t1_List, t2_List]: Takes two templates t1 and t2, and returns the replacement rules that could be applied to t1 or t2 in order to find an intersection of both. Returns {} if there is no possible intersection, or {{}} if they are both templates are the same. 
ReplacementRules[t1_List, t2_List, k_Integer]: Takes two templates t1 and t2, and returns the replacement rules that could be applied to t1 or t2 in order to find an intersection of both. Assumes both templates are modular.
";


Begin["`Private`"];

EquationSystem[template1_List,template2_List]:=
    Equal @@ # & /@ Transpose[{template1, template2}];

DualEquationSystem[template1_List, template2_List] :=
    Module[{notTemplate2, equationSystem, cleanEquationSystem, dualEquationSystem, dualEquationSystemResult},
    (*notTemplate2 = 1 - template2; (*Change 0 to 1 and 1 to 0*);*)
      equationSystem = EquationSystem[template1, template2];(*Make a equation System*)
      cleanEquationSystem = Select[equationSystem, !TrueQ[#]&];(*Remove tautologies*)
      dualEquationSystem = Map[Part[#, 1] == 1 - (Part[#, 2])&, cleanEquationSystem];(*Apply Not to all variables*)
      dualEquationSystemResult = Or @@ (# & /@ dualEquationSystem) (*Change boolean operator AND to OR*)
    ];

DifferenceReplacementRules[template1_, template2_, modulus_Integer : 0] :=
    Module[{
      templateVars = SortBy[Union[Flatten[RuleTemplateVars[#] & /@ {template1, template2}, 1]],
        FromDigits[StringDrop[SymbolName[#], 1]] &],
      dualEquationSystem
    },
      dualEquationSystem = DualEquationSystem[template1, template2];
      If[dualEquationSystem =!= False,
        If[modulus === 0,
          Quiet[Solve[DualEquationSystem[template1, template2], templateVars]] // Union,
          Quiet[Solve[DualEquationSystem[template1, template2], Reverse[templateVars], Modulus -> modulus]] // Union
        ], {}
      ]
    ];

End[];
EndPackage[];
