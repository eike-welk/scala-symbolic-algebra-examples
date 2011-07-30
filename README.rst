===============================================================================
                  Simple Symbolic Mathematics for Scala
===============================================================================

This project contains a very simple, and incomplete, symbolic math library in 
Scala. It can *differentiate* and *evaluate* simple mathematical expressions. 
The library also contains some aspects of an internal DSL: The expressions can 
be entered like regular math with ``Int`` or ``Double`` numbers, and there is 
a ML style *"let" expression*. Here is a short example that demonstrates the 
differentiation feature::

    import symathm.Expression._
    import symathm.ExprOps._
    
    //Create some symbols (unknown variables).
    val (a, x) = (Sym("a"), Sym("x"))

    //Create an expression. `~^` denotes exponentiation (power).
    val expr1 = a * x~^4 + 5 * x~^2 + x~^0.5 

    //Differentiate the expression with respect to `x`.
    val dexpr1 = diff(expr1, x) 

    //Print the expression in human readable form.
    //Prints: "4.0 * a * x ~^ 3.0 + 10.0 * x + 0.5 * x ~^ -0.5;;"
    pprintln(dexpr1)

The library is not intended to be used seriously. Instead it should demonstrate 
simple features of Scala that are interesting for programmers that come form 
traditional object oriented languages; such as: C++, Java, Python, Ruby.
The project should especially demonstrate the usefulness of pattern matching.
Therefore this library is implemented three times with different programming 
paradigms, but with identical features and interfaces:

=====================  =====================================  
Package ``symathm`` :  Functional, with pattern matching.     
Package ``symathv`` :  Object oriented with Visitor pattern.  
Package ``symathoo``:  Classical object oriented.             
=====================  =====================================  

The three libraries are big enough (500 to 700 lines) to give an impression 
how working with a real program would be. But they are small and simple 
enough, to be easily understood. To write the algorithms, and to verify their 
correctness, only high school math is necessary. In principle the algorithms 
can be looked up in Wikipedia 
(http://en.wikipedia.org/wiki/Table_of_derivatives).


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

* Implement a node for a ``for`` loop. Write evaluation and differentiation
  algorithms for it. (I believe differentiating a ``for`` loop is possible, 
  because older versions of *Maple* could do it.)

* Implement an algorithm to distribute factors over sums, and distribute 
  powers over products. For example: ``(a + b) * c`` --> ``a*c + b*c``. 
  This is interesting for ``eval``: more operators with only numeric arguments 
  can be found, and evaluated. 

* Implement an algorithm to collect factors and powers. (The opposite of the 
  algorithm above.) It makes formulas look good.

* Maybe add a separate ``simplify`` function.
* Implement some of the TODOs in the code.


Architecture
============

Data Structures
---------------

All important data structures are defined in object ``Expression``.

Mathematical formulas are internally represented as nested trees of *nodes*. 
They are implemented as *case classes*, syntactical sugar for simple classes
that are intended to work with the ``match`` statement.
(http://www.artima.com/pins1ed/case-classes-and-pattern-matching.html)

* ``Expr``                           : The common base class of all nodes
* ``Num(num: Double)``               : A number (floating point)
* ``Sym(name: String)``              : A variable (symbol)
* ``Add(summands: List[Expr])``      : Addition (n-ary)
* ``Mul(factors: List[Expr])``       : Multiplication (n-ary)
* ``Pow(base: Expr, exponent: Expr)``: Exponentiation (operator ``~^``)
* ``Log(base: Expr, power: Expr)``   : Logarithm
* ``Let(name: String, value: Expr, exprNext: Expr)``: Bind a value to a 
  variable, and put a single expression into the environment, where the new 
  variables are visible.
* ``Asg(lhs: Expr, rhs: Expr)``      : ``:=`` operator, helper object to 
  create ``Let`` nodes.

``1+a`` and ``1+a*2`` are respectively expressed as::

    Add(List(Num(1.0), Sym("a")))
    Add(List(Num(1.0), Mul(List(Sym("a"), Num(2.0)))))

N-Ary Addition and Multiplication
.................................

Addition and multiplication are n-ary, they can have an arbitrary number of 
arguments. ``1 + a + 2 + 3`` and ``1 * a * 2 * 3`` are respectively 
expressed as::

    Add(List(Num(1.0), Sym("a"), Num(2.0), Num(3.0)))
    Mul(List(Num(1.0), Sym("a"), Num(2.0), Num(3.0)))
    
There are no nodes for subtraction or division. Subtraction is represented 
as multiplication with ``-1``: (``-a = -1 * a``). Division is expressed as a 
power of ``-1``: (``1/a = a~^(-1)``). [#maxima]_. 

As there are no subtraction or division operators, ``a-x`` and ``a/x`` are 
respectively expressed as::

    Add(List(Sym("a"), Mul(List(Num(-1.0), Sym("x")))))
    Mul(List(Sym("a"), Pow(Sym("x"), Num(-1.0))))

Let Expressions
................

``Let`` nodes are similar to assignment statements in imperative programming 
languages. They are commands to create a new *environment*, where 
a variable is bound to a value. The dependent expression (the third argument of 
``Let``) is evaluated in this new environment. The value of a ``Let`` node is
the value of its dependent expression.

``Let`` nodes are created by a little abuse of Scala's liberal syntax 
(the DSL): ``let (a:=2) in a*a`` results in::

    Let("a", Num(2.0), Mul(List(Sym("a"), Sym("a"))))

The ``Let`` node does not create the new environment by itself, it is 
interpreted by an algorithm. The ``eval`` algorithm interprets ``Let`` 
nodes as described above: It creates a new environment that contains the new
variables and also the variables of the old environment. Then ``eval``
evaluates the dependent expression in the new environment. The expression 
above would be evaluated to ``Num(4)``.

The *environment* that stores the bindings between variables and their values,
is (currently) implemented as a ``Map[String, Expr]``.

For convenience there are *type* (and *companion object*) ``Environment``, 
and a call-able object ``Env`` to create environments.   

DSL
---

The library contains a modest attempt to implement a domain specific language
(DSL). The implementation of the DSL is in object ``Expression``.
Additionally there is small example program to illustrate the same technique: 
``src/pattern/testdsl.scala``.

The common base class of all nodes, ``Expr``, contains the usual mathematical 
operators: ``+ - * / ~^``, and additionally ``:=``. (``~^`` is the 
exponentiation operator.) Each operator returns a part of the tree. The ``+`` 
operator, for example, returns an ``Add`` node.

There are implicit conversion functions (``int2Num``, ``double2Num``) in 
``Expression``, that convert ``Int`` and ``Double`` objects into ``Num`` 
nodes. This way numbers and nodes can be freely mixed.

``Let`` nodes can be somewhat elegantly created with the call-able helper 
object ``let``. When called with multiple assignments, ``let`` creates nested
``Let`` nodes. The syntax is::

    let (a := 2, x := 3) in a + x

Which returns::
    
    Let("a", Num(2.0), Let("x", Num(3.0), Add(List(Sym("a"), Sym("x")))))

The ``eval`` algorithm would evaluate this expression to ``Num(5)``.

Algorithms
----------

The high-level algorithms are implemented in object ``ExprOps`` (in all 
implementations of the library). All algorithms traverse a tree of nodes in 
a recursive way, and create a new tree as the result.

There are currently two algorithms:

``eval``
    This algorithm behaves like an interpreter of a programming language.

    Differently to a traditional programming language there are no "unknown 
    variable" errors. The algorithm replaces known variables (symbols) by their 
    values, but unknown variables are left unchanged.

    ``eval`` performs the usual arithmetic operations on numbers. Expressions
    that contain numbers and unknown variables are simplified as much as 
    possible. The n-ary multiplication and addition nodes simplify this
    task: ``1 + a + 2`` can easily be simplified to ``3 + a``, by partitioning
    the summands into numbers and other nodes.

``diff``
    Differentiate expressions.

    The algorithm can differentiate ``Let`` nodes; differentiating::
     
        let (a:=f) in g

    with respect to ``x``, basically yields::

        let (a:=f, a$x:=diff(f, x)) in diff(g, x)


.. [#maxima] The ideas for n-ary operators (additon, multiplication), and 
   for the ommission of subtraction and division nodes, were taken from the 
   computer algebra program *Maxima*. It is intended to simplify the 
   algorithms.

