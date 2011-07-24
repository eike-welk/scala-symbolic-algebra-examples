#! /bin/bash
#Run all tests. Compiles all source files before testing.

#Stop at first error
set -e

#Compile all source files 
./make-compile.sh

#Run all tests
scala -classpath bin/ symathm.SymbolicMainM
scala -classpath bin/ symathv.SymbolicMainV
scala -classpath bin/ symathoo.SymbolicMainOo
scala -classpath bin/ UseTheLibraries
scala -classpath bin/ pattern.TestDsl
scala -classpath bin/ pattern.TestVisitor
