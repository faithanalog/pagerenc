#POCSAG Encoder In Haskell

POCSAG encoder written in haskell for fun. Check out
https://github.com/unknownloner/pocsag-encoder for a better-documented
implementation, as this one most certainly is not well documented right now.

#Building

First, install the stack build tool. See https://www.haskellstack.org/ for
details.

Then, from the project root folder:

    stack setup #Downaloads/installs the Haskell compiler if needed
    stack build #Recursively build the project and all dependencies


#Running

Stack will tell you where the executable file is located. You can either copy
that wherever you want it, or run

    stack exec pagerenc

to execute it from the project directory.
