(* ::Package:: *)

BeginPackage["CATemplates`TemplateOperations`TemplateIntersection`", "CATemplates`Basic`"];


Begin["`Private`"];


RawTemplate::usage="RawTemplate[t_List]: Receives a template t, and drops any special sintax construct from it. Currently, it removes expressions of the form x \[Element] {__}.";
RawTemplate[template_]:=
template /. Element[x_,set_] -> x;


ImprisonmentExpressions::usage="ImprisonmentExpressions[t_List]: Receives a template t, and returns all of the expressions of the form x \[Element] {__}.";
ImprisonmentExpressions[template_List]:=
  Cases[template, x_ \[Element] set_ ,Infinity]


ValueRestrictions::usage = "ValueRestrictions[imprisonmentExpression_]: Returns an expression that represents the value restricions dictated by an ImprisonmentExpression. Example: ValueRestrictions[x1 \[Element] {0,1}] -> x1 == 0 || x1 == 1";
ValueRestrictions[imprisonmentExpression_]:=
 Apply[Or,imprisonmentExpression[[1]] == #&/@ imprisonmentExpression[[2]]];


Equations::usage="Equations[t1_List, t2_List]: Receives two templates, t1 and t2, and returns an equation system in which every slot of t1 is equal to the corresponding slot in t2.";
Equations[template1_List,template2_List]:=
Equal @@ # & /@ Transpose[{template1, template2}];


ReplacementRules[template1_, template2_]:=
With[{
templateVars = Union[Flatten[RuleTemplateVars[#] & /@ {template1, template2}, 1]]
},
Quiet @ Solve[Equations[template1, template2], templateVars] 
]


RawTemplateIntersection::usage= "RawTemplateIntersection[t1_List, t2_List]: Receives two templates t1 and t2, and finds a third template that represents their intersection. Both arguments should be raw templates, i.e. templates that don't have special sintax constructs.";
RawTemplateIntersection[template1_, template2_] :=
With[{replacementRules = ReplacementRules[template1, template2]},
If[replacementRules == {},
{},
template1 /.replacementRules]];


TemplateIntersection::usage= "TemplateIntersection[t1_List, t2_List]: Receives two templates t1 and t2, and finds a third template that represents their intersection.";
TemplateIntersection[template1_, template2_] :=
With[{
rawTemplate1 = RawTemplate[template1],
rawTemplate2 = RawTemplate[template2],
imprisonmentExpressions = Join[ImprisonmentExpressions[template1], ImprisonmentExpressions[template2]]
},
RawTemplateIntersection[rawTemplate1, rawTemplate2]];


(* The intersection between two sets of templates is given by the outer product of the intersection over the sets. *)
TemplateIntersection[x_ /; MatchQ[x, {{__} ..}], y_ /; MatchQ[y, {{__} ..}]] :=
  Select[Flatten[TemplateIntersection[#[[1]], #[[2]]] & /@ Flatten[Outer[{#1, #2} &, x, y, 1], 1], 1], (# != {} && ValidTemplateQ[#]) &];


End[];
EndPackage[];
