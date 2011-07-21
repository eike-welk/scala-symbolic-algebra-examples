===============================================================================
                  Simple Symbolic Mathematics for Scala
===============================================================================

This project contains a very simple, and incomplete, symbolic math library in 
Scala. It can *differentiate* and *evaluate* simple mathematical expressions. 
The library also contains some aspects of an internal DSL: The expressions can 
be entered like regular math with ``Int`` or ``Double`` objects, and there is 
a ML style *"let" expression*. Here is a short example that demonstrates the 
differentiation feature::

    import symathm.Expression._
    import symathm.ExprOps._
    
    //Create some symbols (unknown variables)
    val (a, x) = (Sym("a"), Sym("x"))

    //Create an expression. `~^` denotes exponentiation (power).
    val expr1 = a * x~^4 + 5 * x~^2 + x~^0.5 

    //Differentiate the expression with respect to `x`
    val dexpr1 = diff(expr1, x) 

    //Print the expression in human readable form.
    //Prints: "4.0 * a * x ~^ 3.0 + 10.0 * x + 0.5 * x ~^ -0.5;;"
    pprintln(dexpr1)

The library is not intended to be used seriously. Instead it should demonstrate 
features of Scala that are interesting for programmers that come form 
traditional object oriented languages; such as: C++, Java, Python, Ruby.
The project should especially demonstrate the usefulness of pattern matching.
Therefore this library is implemented three times with different programming 
paradigms, but with identical features and interfaces:

=====================  =====================================  
Package ``symathm`` :  Functional, with pattern matching.     
Package ``symathv`` :  Object oriented with Visitor pattern.  
Package ``symathoo``:  Classical object oriented.             
=====================  =====================================  


Repository Contents
===================

``src/``
    ``symathm/SymbolicMainM.scala``
        Symbolic math library, implemented in functional fashion, with pattern 
        matching. 
        (Package: ``symathm``)
    ``symathv/SymbolicMainV.scala``
        Implementation of the library with the visitor pattern. Object 
        oriented, but structure similar to pattern matching. 
        (Package: ``symathv``) 
    ``symathoo/SymbolicMainOO.scala``
        The symbolic math library implemented in simple object oriented fashion.
        (Package: ``symathoo``)

    ``UseTheLibraries.scala``
        Program that demonstrates the features of the libraries.
    
        The three implementations of the library have identical features and 
        interfaces.

    ``pattern/`` 
        ``testdsl.scala``
            Short example implementation of the libraries' "DSL" features.
        ``testvisitor.scala``
            Short example implementation of the visitor pattern. 

``make-compile.sh``
    Compile all Scala source files.
``make-scaladoc.sh``
    Create API documentation with ``scaladoc``.
``README.rst``
    This file.    


Usage
=====

Getting the Software
--------------------

Either get the software by cloning the repository with Mercurial::

  hg clone https://bitbucket.org/eike_welk/scala-symbolic-algebra-test
  
Or download (and extract) one of the auto-generated archives from here:

  https://bitbucket.org/eike_welk/scala-symbolic-algebra-test/downloads
  
Without IDE
-----------

Run the script ``make-compile.sh`` to compile all source files. This might 
take a minute or two:: 

  ./make-compile.sh

Then run any object with a ``main`` method. Start with the usage example
``UseTheLibraries``, which explains all of the libraries' (few) features::

  scala -classpath bin/ UseTheLibraries

To start Scala's read-eval-print loop, you need to specify the ``classpath`` 
where the compiled files are found (but don't specify an object that should 
be run)::

  scala -classpath bin/ 

With IDE
--------

The `Sala IDE for Eclipse` at least, finds the source files and compiles them
automatically. You can run any file that contains an object with a ``main`` 
method by clicking the *Run* button.

First try out the usage example ``UseTheLibraries.scala``.


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

Either a working *Scala* installation (programs ``scalac`` and ``scala``) on a 
Unix-like operating system. (On Windows you have to come up with the right 
command to compile the sources and run them yourself.)

Or even better a *IDE with Scala support*, for example the Scala-IDE for 
Eclipse. 

  http://www.scala-ide.org/


Projects
========

To compare the characteristics of the different programming paradigms, you can 
add features to each version of the library. 

* Add derivation of the ``Log`` node.
* Add new nodes, for example ``sin``, ``cos`` and ``tan``.
* Add function call node. Maybe this makes an inert ``diff`` node superfluous.
  (See point below.)
* Add ``lambda`` (function body) node.
* Implement an inert ``diff`` node. The "a$x" notation is a hack.

* Implement an algorithm to distribute factors over sums, and distribute 
  powers over products. For example: ``(a + b) * c`` --> ``a*c + b*c``. 
  
  This is interesting for ``eval``: more operators with only numeric arguments 
  can be found, and evaluated. 

* Implement an algorithm to collect factors and powers. (The opposite of the 
  algorithm above.) It makes formulas look good.
* Maybe add a separate ``simplify`` function.
* Implement some of the TODOs in the code.
