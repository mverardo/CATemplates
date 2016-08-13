(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    FilterOutOfRange[BuildTemplate[2, 1.0, {}], {0,1,0,1,0,1,0,1}] === {0,1,0,1,0,1,0,1}],
  VerificationTest[
    FilterOutOfRange[BuildTemplate[2, 1.0, {}], {-1,1,0,1,0,1,0,1}] === (##) & []],
  VerificationTest[
    FilterOutOfRange[BuildTemplate[2, 1.0, {}], {0,1,0,1,0,1,0,2}] === (##) & []],
  VerificationTest[
    FilterOutOfRange[BuildTemplate[3, 1.0, {}], {1,1,0,2,0,1,0,1}] === {1,1,0,2,0,1,0,1}]}];

PrintReport[report];
