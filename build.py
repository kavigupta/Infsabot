#!/usr/bin/python3

import sys
from os import rename, system, mkdir
from subprocess import call

args = list(sys.argv)[1:]

def removeifex(name):
    if name in args:
        args.remove(name)
        return True
    return False

PROFILING = removeifex("-prof")
NO_OPT = removeifex("-noopt")

if NO_OPT and PROFILING:
    print ("Invalid argument combination: noopt and profiling!")
    sys.exit()

assert not system("cabal configure %s %s"
        % ("--enable-executable-profiling" if PROFILING else "",
           "-f noopt" if NO_OPT else ""))

assert not system("cabal build")

assert not system("mv dist/build/infsabot-build/infsabot-build bin/Build")

try:
    mkdir("bin")
except FileExistsError:
    pass # exists

assert not system("chmod +x bin/Build")

if PROFILING:
    args += ["+RTS", "-xc", "-p", "-RTS"]

call(["bin/Build"] + args)
