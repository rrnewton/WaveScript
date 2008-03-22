#!/usr/bin/env python

import sys
import getopt

optlist, args = getopt.getopt(sys.argv[1:], 'x', ['nodes=', 'time='])

# import os
# if (os.getenv("TOSROOT") == None):
#   raise Exception("TOSROOT environment variable not set")
# sys.path.append(os.getenv("TOSROOT") + "/tos/lib/tossim/")

from TOSSIM import *

t = Tossim([])
# r = t.radio()
# f = open("topo.txt", "r")

# lines = f.readlines()
# for line in lines:
#   s = line.split()
#   if (len(s) > 0):
#     print " ", s[0], " ", s[1], " ", s[2];
#     r.add(int(s[0]), int(s[1]), float(s[2]))

#t.addChannel("RadioCountToLedsC", sys.stdout)
#t.addChannel("BlinkC", sys.stdout)

t.addChannel("WSQuery", sys.stdout)
t.addChannel("Boot", sys.stdout)

# noise = open("meyer-heavy.txt", "r")
# lines = noise.readlines()
# for line in lines:
#   str = line.strip()
#   if (str != ""):
#     val = int(str)
#     for i in range(1, 4):
#       t.getNode(i).addNoiseTraceReading(val)

# for i in range(1, 4):
#   print "Creating noise model for ",i;
#   t.getNode(i).createNoiseModel()

t.getNode(1).bootAtTime(100001);
#t.getNode(2).bootAtTime(800008);
#t.getNode(3).bootAtTime(1800009);

for i in range(0, 300):
  t.runNextEvent()


