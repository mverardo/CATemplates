(* Mathematica source file  *)
(* Created by IntelliJ IDEA *)
(* :Author: mverardo *)
(* :Date: 26/07/15 *)

<< CATemplates`;

report = TestReport[{
  VerificationTest[
    FilterNotAllowed[BuildTemplate[2, 1.0, {x1,x0}], {1,1}] === {1,1}],
  VerificationTest[
    FilterNotAllowed[BuildTemplate[2, 1.0, {x1 \[Element] {0},x0}], {1,1}] === (##) & []],
  VerificationTest[
    FilterNotAllowed[BuildTemplate[2, 1.0, {x1 \[Element] {0},x0}], {0,1}] === {0,1}],
  VerificationTest[
    FilterNotAllowed[BuildTemplate[3, 1.0, {x1 \[Element] {0, 2}, x0}], {0, 1}] === {0, 1}],
  VerificationTest[
    FilterNotAllowed[BuildTemplate[3, 1.0, {x1 \[Element] {0, 2}, x0}], {1, 1}] === (##) & []],
  VerificationTest[
    FilterNotAllowed[BuildTemplate[3, 1.0, {x1 \[Element] {0, 2}, x0}], {2, 1}] === {2, 1}]
}];

PrintReport[report];
