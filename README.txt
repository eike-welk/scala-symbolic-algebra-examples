===============================================================================
                  Simple Symbolic Mathematics for Scala

             Explore some nice features of the Scala language.
===============================================================================

This project contains a very simple, and incomplete symbolic math library in 
Scala. It can differentiate and evaluate simple mathematical expressions. 
The library also contains some aspects of an internal DSL: The expressions can 
be entered like regular math with Int or Double objects, and there is a ML 
style `let` expression.

The library is not intended to be seriously used. Instead it should demonstrate 
features of Scala that are interesting for programmers that come form 
traditional object oriented languages; such as: C++, Java, Python, Ruby.
The project should especially demonstrate the usefulness of pattern matching.
Therefore this library is implemented three times with different programming 
paradigms:

Functional, with pattern matching. 
    Package: `symathm` 

Classical object oriented. 
    Package: `symathoo`

Object oriented with the Visitor pattern. 
    Package: `symathv`


Usage
=====

Without IDE
-----------

Run the script `make-compile.sh` to compile all source files. Then run any 
object with a `main` method. For example the usage example `UseTheLibraries`::

  ./make-compile.sh
  scala -classpath bin/ UseTheLibraries

With IDE
--------

The Sala IDE for Eclipse at least, finds the source files and compiles them 
automatically. You can run any file that contains an object with a `main` 
method by clicking the `Run` button.

First run the usage example `UseTheLibraries.scala`.


Required Knowledge of Scala
===========================

Only little knowledge of Scala is needed to understand the code. A good 
introductory text on Scala is:

  http://www.artima.com/scalazine/articles/steps.html
  
The text above unfortunately does not cover pattern matching, which is IMHO 
one of Scala's main attractions. Pattern matching is covered here:
 
  http://www.artima.com/pins1ed/case-classes-and-pattern-matching.html


Required Software
=================

Either a working Scala installation (programs `scalac` and `scala`) on a 
Unix-like operating system. (On Windows you have to come up with the right 
command to compile the sources and run them yourself.)

Or even better a IDE with Scala support, for example the Scala-IDE for Eclipse. 

  http://www.scala-ide.org/


Projects
========

To try out the characteristics of the different programming paradigms, you can 
add features to each version of the library. 

* Add derivation of the `Log` node.
* Remove the unnecessary `Neg` node. Replace: -a with: (-1) * a; but look for 
  this pattern in the pretty printer.
* Add new node, for example `sin`, `cos` and `tan`.
* Add new simplification algorithms. Especially add a separate simplify function.
* Add function call node. 
* Add `lambda` (function body) node.
* Implement an inert `diff` node. The "a$x" notation is a hack.
* Implement some of the TODOs
