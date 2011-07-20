===============================================================================
                  Simple Symbolic Mathematics for Scala
===============================================================================

This project contains a very simple, and incomplete symbolic math library in 
Scala. It can *differentiate* and *evaluate* simple mathematical expressions. 
The library also contains some aspects of an internal DSL: The expressions can 
be entered like regular math with Int or Double objects, and there is a ML 
style *"let" expression*.

The library is not intended to be seriously used. Instead it should demonstrate 
features of Scala that are interesting for programmers that come form 
traditional object oriented languages; such as: C++, Java, Python, Ruby.
The project should especially demonstrate the usefulness of pattern matching.
Therefore this library is implemented three times with different programming 
paradigms:

=====================  =====================================  
Package: ``symathm``   Functional, with pattern matching.     
Package: ``symathoo``  Classical object oriented.             
Package: ``symathv``   Object oriented with Visitor pattern.  
=====================  =====================================  


Package Contents
================

``src/symathm/SymbolicMainM``
    Symbolic math library, implemented in functional fashion, with pattern 
    matching. 
    (Package: ``symathm``)
``src/symathv/SymbolicMainV``
    Implementation of the library with the visitor pattern. - Object oriented,
    but structure similar to pattern matching. 
    (Package: ``symathv``) 
``src/symathoo/SymbolicMainOO``
    The symbolic math library implemented in simple object oriented fashion.
    (Package: ``symathoo``)

``src/pattern/testdsl.scala``
    Short example implementation of the libraries' "DSL" features.
``src/pattern/testvisitor.scala``
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
* Add new simplification algorithms. Especially add a separate ``simplify`` 
  function.
* Add function call node. Maybe this makes an inert ``diff`` node superfluous.
  (See point below.)
* Add ``lambda`` (function body) node.
* Implement an inert ``diff`` node. The "a$x" notation is a hack.
* Implement some of the TODOs
