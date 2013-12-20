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
        t_ = t.split("_")[0]
        runStr = "echo \"" + inp + "\" | ./Paladim -ti " + rootdir + t_ + ".pal"
        print "Interpreting " + rootdir + t + " : " + str(test(runStr, outp))

        compStr = "echo \"" + inp + "\" | ./marsrun " + rootdir + t_
        print "Compiling    " + rootdir + t + " : " + str(test(runStr, outp))

def readTest():
    readtestRes = """Welcome to the Echo program, now lets play a game.
I will ask you for an input of a specific type
and echo your answer if it is correct.
Due to the type checking, if you fail I fail!!!

Now, give me a boolean value, represented by a number
input: echo 1

Now, give me an integer value
input: echo 2

Now, give me a char
input: echo 2

Congrats, you won the game
"""

    tests = {"functions/readtest"             : ("2\n2\n2\n",readtestRes)
            ,"types/testTypeInferenceRead_b1" : ("3\n97\na\ng\n4\n","n = 5\nb = 1\nc = g\nb = 1\n")
            ,"types/testTypeInferenceRead_b0" : ("2\n92\na\n?\n0\n","n = 4\nb = 0\nc = ?\nb = 0\n")
            }

    runTests(tests,"")

def procTest():
    tests = {"procSwap"        : ("1\n2\n","1 2\n2 1\n")
            ,"proctest"        : ("", "3 4")
            ,"procReturnSimul" : ("g\n","g\nh\n")
            ,"procReturnSimul" : ("3\n","3\n4\n")
            }

    runTests(tests,"functions/")

def arrayTest():
    tests = {"arrays/fooArray"        : ("","2\n")
            ,"arrays/new"             : ("","")
            ,"arrays/testIndexSimple" : ("","1234\n")
            ,"fib/fibArray"           : ("5\n","8\n")
            }

    runTests(tests,"")

def ifTest():
    ifRes = """test 1 expects result -1: ~1
test 2 expects result 0: 0
test 3 expects result 1: 1

test 4 expects result 6: 6
test 5 expects result -40: ~40
test 6 expects result 15: 15
"""
    tests = {"dangling"   : ("","passed.\n")
            ,"iftheltest" : ("",ifRes)
            }

    runTests(tests,"ifthenelse/")

def opTest():
    mulDivRes = """Test 1 expects 12 and was: 12
Test 2 expects 720 and was: 720
Test 3 expects 60 and was: 60
"""

    assocRes = """passed.
passed.
passed.
passed.
passed.
passed.
passed.
passed.
passed.
"""

    tests = {"operators/logical"    : ("","Test result should be 1. Actual result: 1")
            ,"operators/orTest"     : ("","1")
            ,"operators/testMulDiv" : ("",mulDivRes)
            ,"misc/precAssoc"       : ("",assocRes)
            }

    runTests(tests,"")

def fibTest():
    tests = {"fibRec_7"   : ("7\n","21\n")
            ,"fibWhile_7" : ("7\n","21\n")
            ,"fibArray_7" : ("7\n","21\n")
            ,"fibRec_9"   : ("9\n","55\n")
            ,"fibWhile_9" : ("9\n","55\n")
            ,"fibArray_9" : ("9\n","55\n")
            }

    runTests(tests,"fib/")

def main():
    tests = {"arrays"     : arrayTest
            ,"procedures" : procTest
            ,"read"       : readTest
            ,"if"         : ifTest
            ,"operators"  : opTest
            ,"fib"        : fibTest
            }

    def allTests():
        for (n,f) in tests.items():
            print "TEST - " + n + ":"
            f()
            print

    if len(argv) == 1:
        allTests()
    elif "-l" in argv:
        print "Test groups:"
        for n in tests.keys():
            print n
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
