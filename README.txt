-------------------------------------------------------------------------------
                  Simple Symbolic Mathematics for Scala

           A demonstration of different programming paradigms.
-------------------------------------------------------------------------------

This project contains a very simple, and incomplete symbolic maths library in 
Scala. Additionally it contains some aspects of an internal DSL to make the 
creation of mathematical expressions relatively painless. 

The library is not intended to be seriously used. Instead it should demonstrate 
features of Scala that are interesting for programmers that come form 
traditional object oriented languages; such as: C++, Java, Python, Ruby.
Especially should the library demonstrate the usefulness of pattern matching.
Therefore this library is implemented three times with different programming 
paradigms:

Functional, with pattern matching. 
    Package: `symathm` 

Classical object oriented. 
    Package: `symathoo`

Object oriented with the Visitor pattern. 
    Package: `symathv`


Features
--------

* Evaluation of mathematical expressions with known and unknown variables.
* Differentiation of expressions.
* A very simple internal DSL::

    val x = Sym("x")
    pprintln(diff(x**2 + x + 2, x))

  Prints:
    1 + 2 * x

  The "$" character in variable names denotes derivation: a$x = da/dx
  The following snippet demonstrates the product rule. (The values of a$x 
  and b$x don't matter, the derivation algorithm doesn't look at them.)::
   
    val (a, b, x) = (Sym("a"), Sym("b"), Sym("x"))
    val env = Environ("a$x"->0, "b$x"->0) 
    pprintln(diff(a * b, x, env))
  
  Prints:
    a$x * b + a * b$x


Projects
--------

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
