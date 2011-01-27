#!/bin/bash

# [2008.09.19] This is a very ugly way to read bytes off the serial
# port.  However, I can't seem to get minicom or screen to work
# non-interactively to dump those bytes.  And there's something that
# screen is doing to set up the serial port that enables 'cat' to
# subsequently work.

# First argument should be 
if [ -c "$1" ]
then USB=$1
else USB=/dev/ttyUSB0
fi

stty -F $USB 115200
screen $USB < /dev/null > /dev/null &
#screen $USB &
SCRN=$!

# Wow, hack of hacks, this doesn't work if we sleep!
#sleep 1 
#ps aux | grep screen
#echo Screen process $SCRN

kill $SCRN 

stty -F $USB 115200
#stty -F $USB 
cat < $USB
