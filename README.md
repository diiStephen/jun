# Term-Rewriter
The goal of this project is to develop an interative user interface for automated theorem proving using rewriting approaches via completing a set of equational axioms to a convergent term rewriting system, and to present a "scripting language" for building, testing, and researching algorithms, data structures, and procedures for automated deduction, theorem proviing, and unification. 

# Prerequisties 
Term-rewriter is built with [Haskell Stack](https://docs.haskellstack.org/en/stable/README/). The easiest way to install Haskell Stack is with the following curl command 

    curl -sSL https://get.haskellstack.org/ | sh

where more details are available from the official website listed above. 

# Building 
Clone this repository and use the 

    stack build 

command in the root directory of the repository. This will compile the modules and install the (small number of) necessary dependencies 

# Running the Interactive Mode
There are two ways to use Term-Rewriter: Interactive mode and as a scripting language. This section focuses on interative mode. After running the build step above, use the exec command to run the term-rewriter-exe executable 

    stack exec term-rewriter-exe

# Scripting 
See the ```src/Examples``` module for examples of using Term-Rewriter as a domain specific language. 

# Unit Tests
Run the

    stack test

command in the root directory of the project to run unit tests. 