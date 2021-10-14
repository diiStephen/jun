# Jun (純)
This repository contains Jun, a Haskell based tool for term rewriting and automated deduction. 

# Prerequisties 
Jun is built with [Haskell Stack](https://docs.haskellstack.org/en/stable/README/). The easiest way to install Haskell Stack is with the following curl command 

    curl -sSL https://get.haskellstack.org/ | sh

where more details are available from the official website listed above. 

# Building 
Clone this repository and use the 

    stack build 

command in the root directory of the repository. This will compile the modules and install the (small number of) necessary dependencies 

# Running the Interactive Mode
There are two ways to use Jun: interactive mode and as a scripting language. This section focuses on interactive mode. After running the build step above, use the exec command to run the jun executable 

    stack exec jun

# Scripting 
See the ```src/Examples``` module for examples of using Jun as a domain specific language. 

# REPL 
All of Jun's module can be loaded into the ghci for testing and research via the command: 

    stack repl

we recommend setting the prompt to reduce clutter by e.g. running

    :set prompt "ghci: "

after loading the repl. 

# Unit Tests
Run the

    stack test

command in the root directory of the project to run unit tests. 