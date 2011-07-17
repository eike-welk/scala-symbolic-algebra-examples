#! /bin/bash
# Compile all Scala source files in "src/" and put the resulting class 
# files into directory "bin/".

rm -rf bin
mkdir bin

#scalac -verbose -d bin/ `find . -name "*.scala"`
scalac -d bin/ `find . -name "*.scala"`

#Run the resulting class files:
#   scala -classpath bin/ UseTheLibraries
#
#   scala -classpath bin/ symathm.SymbolicMainM
#   scala -classpath bin/ symathv.SymbolicMainV
#   scala -classpath bin/ symathoo.SymbolicMainOo
