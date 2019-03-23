# Pager Encoder In Haskell

POCSAG/FLEX encoder written in Haskell for fun. Check out
https://github.com/unknownloner/pocsag-encoder for a more thoroughly documented
implementation of a POCSAG encoder.

## Building

First, install the stack build tool. See https://www.haskellstack.org/ for
details.

Then, from the project root folder:

    stack setup    # Downloads/installs the Haskell compiler if needed
    stack build    # Recursively build the project and all dependencies


## Installing

Stack will tell you what folder the executable file is located. For example:

    Installing executable(s) in /data/pagerenc/.stack-work/install/x86_64-linux/lts-6.10/7.10.3/bin

You may either copy the file from that folder to wherever you want, or run

    stack install

to automatically install the executable in $HOME/.local/bin    

## Running

Execute the program directly from the file installed in the previous step, or run

    stack exec pagerenc -- [flags]

to execute it from the project directory.

## Usage

pagerenc reads a series of commands from STDIN and writes 16-bit PCM output to
STDOUT at a sample rate of 22050 Hz.

To make a wav file instead, try something like this (requires ffmpeg):

    cat pager_commands | pagerenc | ffmpeg -f pcm_s16le -ar 22050 -ac 1 -i - -ar 44100 output.wav

To encode a single message, you could do this

    echo 'FLEX:ID:Message' | pagerenc | ffmpeg -f pcm_s16le -ar 22050 -ac 1 -i - -ar 44100 output.wav

Each line may contain either a message to be encoded in a specific format, or
a delay command to insert noise of an optionally specified duration. Valid
formats are POCSAG512, POCSAG1200, POCSAG2400, and FLEX. Messages are in the
format of PROTOCOL:ADDRESS:MESSAGE

Here's an example.

    FLEX:1:Hello
    FLEX:2:World
    WAIT
    POCSAG512:30:Some pocsag512 message
    POCSAG2400:35:Some pocsag2400 message
    WAIT 30
    POCSAG1200:8:Some pocsag1200 message

multimon-ng will not decode the first FLEX message sent in a batch of FLEX
messages. It uses the first message to synchronize the decoder, so it can't
decode that message. Any non-FLEX transmission immediately following a FLEX
messages ends the FLEX batch. This inclueds WAITs, and POCSAG messages.

When WAIT is not provided any parameters, it will insert a random delay between
MINDELAY and MAXDELAY, specifiable with command line flags. The default min and
max delay are 1 and 10 seconds respectively.

The --throttle flag will cause pagerenc to output audio data as if it was being
decoded in real time from a radio.
