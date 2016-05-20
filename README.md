# CATemplates

CATemplates is a Mathematica package that enables the use of [Cellular Automaton](http://en.wikipedia.org/wiki/Cellular_automaton) (CA) templates, as first described in [Representing Families of Cellular Automata Rules](http://www.mathematica-journal.com/2014/08/representing-families-of-cellular-automata-rules/).

A Cellular Automaton template allows one to represent potentially huge CA sets through a lightweight data structure, and defer their enumeration to a moment when the rules are really needed.

The `CATemplates` package provides ways to create user-defined raw templates, along with built-in generator functions able to create templates for rule sets which share a given static property. It also includes operations capable of manipulating templates and enumerating the sets they represent.

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

Give execution permission to the "test" script inside the project's root:

    $ chmod +x test

Run the script:

    $ ./test

It checks for every .m file in the "/Tests" directory, and runs it as a mathematica script.

