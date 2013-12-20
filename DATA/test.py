#!/usr/bin/env python
from os import system
from subprocess import check_output
from sys import argv,exit

def getOutp(s):
    return s.split("Input/Output:\n")[-1]

def test(command,outp):
    p = check_output(command, shell=True)
    return getOutp(p) == outp

def runTests(tests,rootdir):
    for (t,(inp,outp)) in tests.items():
        runStr = "echo \"" + inp + "\" | ./Paladim -ti " + rootdir + t + ".pal"
        print "Interpreting " + t + " : " + str(test(runStr, outp))

        compStr = "echo \"" + inp + "\" | ./marsrun " + rootdir + t
        print "Compiling    " + t + " : " + str(test(runStr, outp))


def procTest():
    tests = {"procSwap" : ("1\n2\n","1 2\n2 1\n")
            ,"proctest" : ("", "3 4")
            }

    runTests(tests,"functions/")

def arrayTest():
    tests = {"fooArray"        : ("","2\n")
            ,"new"             : ("","")
            ,"testIndexSimple" : ("","1234\n")
            }

    runTests(tests,"arrays/")

def main():
    tests = {"arrays"     : arrayTest
            ,"procedures" : procTest
            }

    def allTests():
        for (n,f) in tests.items():
            print "TEST - " + n + ":"
            f()
            print

    if len(argv) == 1:
        allTests()
    else:
        for t in argv[1:]:
            if t not in tests:
                print("No such test: " + t)
                exit(1)
            print "TEST - " + t
            tests[t]()
            print


if __name__ == "__main__":
    main()
