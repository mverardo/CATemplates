# CATemplates

A Mathematica package that enables the use of [Cellular Automaton](http://en.wikipedia.org/wiki/Cellular_automaton) templates, as described first in [Representing Families of Cellular Automata Rules](http://www.mathematica-journal.com/2014/08/representing-families-of-cellular-automata-rules/).

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

If you don't want to clone the repository directly inside $UserBaseDirectory, you can clone it to another place and add a SymLink to the $UserBaseDirectory/Applications.

