BeginPackage["CATemplates`TemplateOperations`Intersection`RawIntersection`", {"CATemplates`Basic`", "CATemplates`TemplateOperations`Intersection`Common`"}];

RawIntersection::usage= "RawIntersection[t1_List, t2_List]: Receives two templates t1 and t2, and finds a third template that represents their intersection. Both arguments should be raw templates, i.e. templates that don't have special sintax constructs.";

Begin["`Private`"];

End[]; (* `Private` *)

RawIntersection[templateList1_, templateList2_] :=
    With[{replacementRules = ReplacementRules[templateList1, templateList2]},
      If[replacementRules == {},
        {},
        Union[templateList1 /.replacementRules, templateList2 /. replacementRules]]];

EndPackage[];