BeginPackage["CATemplates`CATemplate`",
  {
    "CATemplates`TemplateOperations`Expansion`RawExpansion`"
  }
];

BuildTemplate::usage=
    "BuildTemplate[k_Integer, r_Real, rawList_List, expansion_Function]
  Builds a template that represents the subspace of the CA space given by k and r, described by the variables in <rawList>.
  <expansion> is the function used by ExpandTemplate to expand the built template.
BuildTemplate[k_Integer, r_Real, rawList_List, expansion_Function, N_Integer]
  <N> is the value of modulus used by the template generator to create <rawList>.";

BaseTemplate::usage="BaseTemplate[k_Integer, r_Real] := Gives the base template for the space of radius r k-ary rules.";

k::usage="k[t_] = Gets the number of possible states (k) for cells of the space represented by template t.";

r::usage="r[t_] = Gets the radius (r) of the family represented by template t.";

kAryRuleTemplate::usage="kAryRuleTemplate[t_] = Gets the kAryRuleTemplate which represents a subset of the base t (including template variables).";

expansionFunction::usage="expansionFunction[t_] = Gets the expansion function used by template t.";

postExpansionFn::usage="postExpansionFn[t_] = Gets the post expansion function used by template t.";

templateMod::usage="templateMod[t_] = Gets a templateMod number used by template t.";

Begin["`Private`"];

BuildTemplate[k_Integer, r_Real, rawList_List] :=
    Association["k" -> k, "r" -> r, "rawList" -> rawList];

BuildTemplate[k_Integer, r_Real, rawList_List, postExpansionFn_] :=
    Association["k" -> k, "r" -> r, "rawList" -> rawList, "postExpansionFn" -> postExpansionFn];

BuildTemplate[k_Integer, r_Real, rawList_List, postExpansionFn_, N_Integer] :=
    Association["k" -> k, "r" -> r, "rawList" -> rawList, "postExpansionFn" -> postExpansionFn, "N" -> N];

BaseTemplate[k_Integer, r_Real] :=
    With[{list = Symbol["x" <> ToString[#]] & /@ Range[(k^(Ceiling[r * 2] + 1)) -1, 0, -1]},
      BuildTemplate[k, r, list, RawExpansion]];

k[t_Association] := t[["k"]];

r[t_Association] := t[["r"]];

templateMod[t_Association] := t[["N"]];

kAryRuleTemplate[t_Association] := t[["rawList"]];

expansionFunction[t_Association] := t[["expansionFunction"]];

postExpansionFn[t_Association] := t[["postExpansionFn"]];

End[];

EndPackage[];
