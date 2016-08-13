(* ::Package:: *)

<< CATemplates`;

FreeVariableQ::usage = "FreeVariableQ[expression_]: Receives an expression, and returns true if the expression is a free variable and false otherwise.";
FreeVariableQ[expression_] := MatchQ[expression, _Symbol];

CorrespondsToNeighborhoodQ::usage = "CorrespondsToNeighborhoodQ[freeVariable_Symbol, nbIndex_Integer]: Receives a free variable expression and a neighborhood index, and returns true if the variable's index corresponds to the received nb index.";
CorrespondsToNeighborhoodQ[symbol_, nbIndex_] :=
    (FromDigits[StringDrop[SymbolName[symbol], 1]] === nbIndex);

PreservesIndexVariableDualityQ::usage = "PreservesIndexVariableDualityQ[template_]: Receives a template and returns true if the template preserver the index-variable diality.";
PreservesIndexVariableDualityQ[template_] :=
    And @@ (MapIndexed[(!FreeVariableQ[#1]) || (CorrespondsToNeighborhoodQ[#1, First[#2] - 1]) &, Reverse[template]]);

report = TestReport[{
  VerificationTest[
    StateConservingTemplate[] === BuildTemplate[2, 1.0, {1,1+x2-x3,1-x2,1-x1-x2,x3,x2,x1,0}, FilterOutOfRange]],
  VerificationTest[
    StateConservingTemplate[3,1.0] === BuildTemplate[3, 1.0, {2,2+x7-x8,2+x6-x8,2+x1-x2+x5-x7,2+x1-x2+x4-x7,2+x1-x2+x3-x7,2-x6,2+x1-x2-x6,2-x2-x6,1-x1+x2-x5+x8,1-x1+x2-x5+x7,1-x1+x2-x5+x6,1-x4+x5,1,1+x3-x4,1-x1+x2-x3,1-x3,1-x1-x3,x8,x7,x6,x5,x4,x3,x2,x1,0}, FilterOutOfRange]],
  VerificationTest[
    StateConservingTemplate[2,2.0] === BuildTemplate[2, 2.0, {1,1+x14-x15,1+x13-x14+x6-x7,1+x12-x14+x6-x7,1+x11-x13+x2-x3+x5-x6,1+x10-x13+x2-x3+x5-x6,1-x12+x2-x3+x4-x6+x9,1-x12+x2-x3+x4-x6+x8,1-x11-x2+x3-x5+x7,1-x11-x2+x3-x5+x6,1-x10,1-x10+x4-x5,1-x2+x3-x4-x9,1-x4-x9,1-x2-x4-x8,1-x1-x2-x4-x8,x15,x14,x13,x12,x11,x10,x9,x8,x7,x6,x5,x4,x3,x2,x1,0}, FilterOutOfRange]],
  VerificationTest[
    ExpandTemplate[StateConservingTemplate[]]=={{1,1,1,1,0,0,0,0},{1,1,1,0,0,0,1,0},{1,0,1,1,1,0,0,0},{1,0,1,0,1,0,1,0},{1,1,0,0,1,1,0,0}}],
  VerificationTest[
    Sort[FromDigits[#, 2] & /@ ExpandTemplate[ModNStateConservingTemplate[2]]] == {132, 150, 170, 184, 204, 222, 226, 240}],
  VerificationTest[
    PreservesIndexVariableDualityQ[ModNStateConservingTemplate[2][["core"]]]],
  VerificationTest[
    PreservesIndexVariableDualityQ[ModNStateConservingTemplate[3][["core"]]]]
}];

PrintReport[report];
