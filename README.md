# CATemplates

CATemplates is a Mathematica package that enables the use of [Cellular Automaton](http://en.wikipedia.org/wiki/Cellular_automaton) (CA) templates, as first described in [Representing Families of Cellular Automata Rules](http://www.mathematica-journal.com/2014/08/representing-families-of-cellular-automata-rules/).

A Cellular Automaton template allows one to represent potentially huge CA sets through a lightweight data structure, and defer their enumeration to a moment when the rules are really needed.

The `CATemplates` package provides ways to create user-defined raw templates, along with built-in generator functions able to create templates for rule sets that share a given static property. It also includes operations capable of manipulating templates and enumerating the sets they represent. 
# Usage

```Mathematica
(* Import the CATemplates package after installation *)
In[0] = <<CATemplates`;

(* Generate a template for all elementary colorblind CAs *)
In[1] = tColorblind = ColorBlindTemplate[2, 1.0];

(* Expand tColorblind to get all 16 rule tables of the colorblind elementary CAs (in k-ary form)*)
In[2] = ExpandTemplate[tColorblind]
Out[2] = {{1,1,1,1,0,0,0,0},{0,1,1,1,0,0,0,1},{1,0,1,1,0,0,1,0},{0,0,1,1,0,0,1,1},{1,1,0,1,0,1,0,0},{0,1,0,1,0,1,0,1},{1,0,0,1,0,1,1,0},{0,0,0,1,0,1,1,1},{1,1,1,0,1,0,0,0},{0,1,1,0,1,0,0,1},{1,0,1,0,1,0,1,0},{0,0,1,0,1,0,1,1},{1,1,0,0,1,1,0,0},{0,1,0,0,1,1,0,1},{1,0,0,0,1,1,1,0},{0,0,0,0,1,1,1,1}}

(* Generate a template for all elementary totalistic rules *)
In[3] = tTotalistic = TotalisticTemplate[2, 1.0];

(* Expand tTotalistic to get all of the 16 totalistic elementary CAs*)
In[4] = ExpandTemplate[tTotalistic]
Out[4] = {{0,0,0,0,0,0,0,0},{0,0,0,0,0,0,0,1},{0,0,0,1,0,1,1,0},{0,0,0,1,0,1,1,1},{0,1,1,0,1,0,0,0},{0,1,1,0,1,0,0,1},{0,1,1,1,1,1,1,0},{0,1,1,1,1,1,1,1},{1,0,0,0,0,0,0,0},{1,0,0,0,0,0,0,1},{1,0,0,1,0,1,1,0},{1,0,0,1,0,1,1,1},{1,1,1,0,1,0,0,0},{1,1,1,0,1,0,0,1},{1,1,1,1,1,1,1,0},{1,1,1,1,1,1,1,1}}

(* Intersect both templates *)
In[5] = tTotalisticAndColorblind = TemplateIntersection[tColorblind, tTotalistic];

(* Expanding the intersection template renders only the elementary rules which are both totalistic and colorblind *)
In[6] = ExpandTemplate[tTotalisticAndColorblind]
Out[6] = {{1,1,1,0,1,0,0,0},{0,1,1,0,1,0,0,1},{1,0,0,1,0,1,1,0},{0,0,0,1,0,1,1,1}}

(* Here is where things get fun!
   We can generate templates for bigger (potentially huge) spaces.
   Let's try to increase r a little: *)
In[7] = $RecursionLimit = Infinity; tr5 = With[{k=2, r=5.0}, TemplateIntersection[ColorBlindTemplate[k, r], TotalisticTemplate[k, r]]];

(* Note we just found templates representatives of all binary radius 5.0 colorblind rules,
   did the same for totalistic rules and intersected the sets.
   All of this executed in 4.52314 seconds on my notebook. 
   Remember there are 2^2^11 = 3.23 10^616 binary, radius 5.0 CAs. 
   Given the new template, all we have to do is a new Expansion to find out how many rules are both totalistic and coloblind in this huge space. *)
In[8] = Length[ExpandTemplate[tr5]]
Out[8]= 64 (* Only 64 of the 2^2048 rules are both colorblind and totalistic. *)
```

# Installation

<!---
http://mathematica.stackexchange.com/questions/303/custom-package-development-basic-steps
--->

Open Mathematica, and evaluate the following code:
    
    $UserBaseDirectory
    
On a terminal, cd to $UserBaseDirectory/Applications, and clone this repository:

    git clone git@github.com:mverardo/CATemplates.git

Restart Mathematica, and run:

    << CATemplates`

The package should be loaded.

If you don't want to clone the repository directly inside $UserBaseDirectory, you can clone it to another place and add a SymLink to $UserBaseDirectory/Applications.

#Running unit tests

Run the script:

    $ ./test

It checks for every .m file in the "/Tests" directory, and runs it as a mathematica script.

