#Pager Encoder In Haskell

POCSAG/FLEX encoder written in haskell for fun. Check out
https://github.com/unknownloner/pocsag-encoder for a more thoroughly documented
implementation of a POCSAG encoder.

#Building

First, install the stack build tool. See https://www.haskellstack.org/ for
details.

Then, from the project root folder:

    stack setup    #Downloads/installs the Haskell compiler if needed
    stack build    #Recursively build the project and all dependencies


#Installing

Stack will tell you what folder the executable file is located. For example:

    Installing executable(s) in /data/pagerenc/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin

You may either copy the file from that folder to wherever you want, or run

    stack install

to automatically install the executable in $HOME/.local/bin    

#Running

Execute the program directly from the file installed in the previous step, or run

    stack exec pagerenc -- [flags]

to execute it from the project directory.
