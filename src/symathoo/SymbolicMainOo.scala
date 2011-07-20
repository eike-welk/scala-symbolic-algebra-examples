/*-------------------------------------------------------------------------+
 |   Copyright (C) 2011 by Eike Welk                                       |
 |   eike.welk@gmx.net                                                     |
 |                                                                         |
 |   License: GPL                                                          |
 |                                                                         |
 |   This program is free software; you can redistribute it and#or modify  |
 |   it under the terms of the GNU General Public License as published by  |
 |   the Free Software Foundation; either version 2 of the License, or     |
 |   (at your option) any later version.                                   |
 |                                                                         |
 |   This program is distributed in the hope that it will be useful,       |
 |   but WITHOUT ANY WARRANTY; without even the implied warranty of        |
 |   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         |
 |   GNU General Public License for more details.                          |
 |                                                                         |
 |   You should have received a copy of the GNU General Public License     |
 |   along with this program; if not, write to the                         |
 |   Free Software Foundation, Inc.,                                       |
 |   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             |
 +-------------------------------------------------------------------------+*/

/**
 * Simple Symbolic Algebra in Scala
 * 
 * This implementation uses '''object oriented''' style,  classes contain 
 * '''data and methods''' that operate on the data.
  */
package symathoo

import scala.math.{ pow, log, E }
import scala.collection.mutable.ListBuffer

/**
 * Define the expression's nodes, and the DSL.
 */
object Expression {
  //The elements of the AST ----------------------------------------------
  /**
   * Common base class of all expression (AST) nodes.
   *
   * == Binary Operators ==
   * 
   * Implements binary operators for the elements of the AST.
   * `Int` and `Double` can be mixed with `Expr` nodes when using binary 
   * operators, because the companion object defines implicit conversions to 
   * [[symathoo.Expression.Num]].
   * 
   * In the following code snippet `myExpr` is an [[symathoo.Expression.Add]]. 
   * {{{
   * val x = Sym("x")
   * val myExpr = 2 * (x~^2) + 2 * x + 3
   * }}}
   * 
   * `Expr` and its subclasses contain several methods that operate on the tree 
   * of expressions. The implementations in this base class mostly do not work,
   * but throw exceptions. (However they are not abstract to be able to test 
   * partial implementations.)
   * 
   * == Pretty Printing ==
   * 
   * `prettyStr`: convert each instance to a pretty printed string.
   * '''Must be overridden in all child classes!'''
   * 
   * `pprintln`: print the instance in pretty printed form, append ";;".
   * 
   * == Simplification ==
   * 
   * `simplify`: Convert this object to a more simple form. Does (should do)  
   * simple algebraic manipulations, and numeric computations. 
   * '''Must be overridden in all child classes!'''
   * 
   * == Differentiation == 
   * 
   * `diff`: Differentiate this object symbolically. 
   *  '''Must be overridden in all child classes!'''
   *  
   * == Evaluation == 
   *  
   * `eval`: Compute the value of a numerical (sub) expression, and substitute 
   * known values. performs the usual arithmetic operations.
   * Terms with unknown symbols are returned un-evaluated. 
   * The known values (the environment) are given in a `Map(name -> expression)`. 
   */
  abstract class Expr {
    //Binary operators.
    def +(other: Expr) = Add(this :: other :: Nil).flatten()
    def -(other: Expr) = Add(this :: other.unary_- :: Nil)
    def unary_-        = Mul(Num(-1) :: this :: Nil)
    def *(other: Expr) = Mul(this :: other :: Nil).flatten()
    def /(other: Expr) = Mul(this :: Pow(other, Num(-1)) :: Nil)
    /** Power operator. Can't be `**` or `^`, their precedence is too low. */
    def ~^(other: Expr) = Pow(this, other)
    def :=(other: Expr) = Asg(this, other)
  
    /**
     * Convert instance to a pretty printed string.
     * '''All child classes must override this method!'''
     */
    def prettyStr(outerNode: Expr = Let("", Num(0), Num(0)) //`Let` has lowest precedence
                 ): String = { 
      throw new Exception("Method is not defined") 
    }
  
    /** Print instance in pretty printed (human readable) form. */
    def pprintln(debug: Boolean = false) = {
      if (debug) {
        println("--- AST ------------------------------------")
        println(this)
        println("--- Human Readable -------------------------")
        println(this.prettyStr() + ";;")
        println()
      } else {
        println(this.prettyStr() + ";;")
      }
      this
    }
  
    /**
     * Convert instance to a more simple form.
     * Does simple algebraic manipulations, and numeric computations.
     */
    def simplify(): Expr = { throw new Exception("Method is not defined") }
  
    /**
     * Differentiate this object symbolically.
     * '''All child classes must override this method!'''
     */
    def diff(x: Sym, env: Environment = Environment()): Expr = {
      throw new Exception("Method is not defined")
    }
  
    /**
     *  Evaluates an expression.
     *  
     * Evaluate an expression in an environment where some symbols are known
     * Looks up known symbols, performs the usual arithmetic operations.
     * Terms with unknown symbols are returned un-evaluated. 
     */
    def eval(env: Environment = Environment()): Expr = {
      throw new Exception("Method is not defined")
    }
  }
  /**
   * Implicit conversions from [[scala.Int]] and [[scala.Double]] to 
   * [[symathoo.Expression.Num]], and helper methods.
   */
  object Expr {
    //implicit conversions so that numbers can be used with the binary operators
    implicit def int2Num(inum: Int) = Num(inum)
    implicit def double2Num(dnum: Double) = Num(dnum)
  
    /**
     * Compute the precedence of each node, for putting parentheses around 
     * the string representation if necessary.
     */
    def precedence(e: Expr) = e match {
      case Asg(_, _) => 1
      case Add(_)    => 2
      case Mul(_)    => 3
      case Pow(_, _) => 4
      case _         => -1   //No parentheses necessary
    }
    
    /**
     * Put parentheses around the string representation of a node if necessary.
     * 
     * @param nodeStr   String representation of `node`
     * @param node      The node that is converted to a string
     * @param outerNode The surrounding node. If this node is a binary operator
     *                  with higher precedence than term, then a pair of 
     *                  parentheses is put around term's string representation.
     */
    def putParentheses(nodeStr: String, node: Expr, outerNode: Expr) = {
      val (precTerm, precOuter) = (precedence(node), precedence(outerNode))
      if (precTerm == -1 || precOuter == -1)  nodeStr
      else if (precTerm < precOuter)          "(" + nodeStr + ")"
      else                                    nodeStr  
    }
  }
  
  //--- The concrete node types --------------------------------------------------
  /** Numbers */
  case class Num(num: Double) extends Expr {
    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = { 
        if (num == E) "E"
        else num.toString()
    }
    /** Returns this object unchanged. */
    override def simplify() = this
    /** Returns this object unchanged. */
    override def eval(env: Environment = Environment()): Expr = this
    /** Differentiate number. Returns 0. */
    override def diff(x: Sym, env: Environment = Environment()): Expr = Num(0)
  }
  
  /** Symbols (references to variables) */
  case class Sym(name: String) extends Expr {
    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = name
    /** Returns this object unchanged. */
    override def simplify() = this
    /** Returns value of variable. Unknown variables are returned unchanged. */
    override def eval(env: Environment = Environment()): Expr = 
      env.getOrElse(name, this)
  
    /**
     * Differentiate variable.
     * The "$" character in variable names denotes derivation: a$x = da/dx
     * This is used for deriving `Let` nodes.
     */
    override def diff(x: Sym, env: Environment = Environment()): Expr = {
          val dName = name + "$" + x.name
          if      (name == x.name)      Num(1)
          else if (env.contains(dName)) Sym(dName)
          else                          Num(0)
        }
  }
  
  /**
   * N-ary addition (+ a b c d).
   * Subtraction is emulated with the unary minus operator
   */
  case class Add(summands: List[Expr]) extends Expr {
    /**
     * Convert nested additions to flat n-ary additions:
     * `(+ a (+ b c)) => (+ a b c)`
     */
    def flatten(): Add = {
      val summandsNew = new ListBuffer[Expr]
      for (s <- this.summands) {
        s match {
          case a: Add => summandsNew ++= a.flatten().summands
          case _      => summandsNew += s
        }
      }
      Add(summandsNew.toList)
    }
    
    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = {
        var sRep = ""
        for (s <- summands) {
          s match {
            case Mul(Num(-1) :: fact :: Nil) => sRep += " - " + fact.prettyStr(this)
            case summand                     => sRep += " + " + summand.prettyStr(this)
          }
        }
        if      (sRep.startsWith(" + ")) sRep = sRep.substring(3)
        else if (sRep.startsWith(" - ")) sRep = "-" + sRep.substring(3)
        else throw new Exception("Internal Error!") 
        
        Expr.putParentheses(sRep, this, outerNode)
    }
  
    /** Simplify a n-ary addition */
    override def simplify(): Expr = {
      //flatten nested Add
      val addF = this.flatten()

      //sum the numbers up, keep all other elements unchanged
      val (nums, others) = addF.summands.partition(t => t.isInstanceOf[Num])
      val sum = nums.map(x => x.asInstanceOf[Num].num)
                    .reduceOption((x, y) => x + y)
                    .filterNot(t => t == 0) //if result is `0` remove it
                    .map(Num).toList
      val sumsNew = sum ::: others
  
      //The only remaining summand was a `0` which was filtered out. 
      if (sumsNew.length == 0) return Num(0)
      //Remove Adds with only one argument:  (+ 23) -> 23
      else if (sumsNew.length == 1) sumsNew(0)
      else Add(sumsNew)
    }
    
    /** Evaluate n-ary addition. */
    override def eval(env: Environment = Environment()): Expr =
      Add(summands.map(t => t.eval(env))).simplify()
  
    /** Differentiate n-ary addition.*/
    override def diff(x: Sym, env: Environment = Environment()): Expr = 
      Add(summands.map(t => t.diff(x, env))).simplify()
  }
  
  /** N-ary multiplication (* a b c d); division is emulated with power */
  case class Mul(factors: List[Expr]) extends Expr {
    /**
     * Convert nested multiplications to flat n-ary multiplications:
     * `(* a (* b c)) => (* a b c)`
     */
    def flatten(): Mul = {
      val factorsNew = new ListBuffer[Expr]
      for (s <- this.factors) {
        s match {
          case m: Mul => factorsNew ++= m.flatten().factors
          case _      => factorsNew += s
        }
      }
      Mul(factorsNew.toList)
    }

    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = {
      val sRep = this match {
        //convert single "-a"
        case Mul(Num(-1) :: fact :: Nil) => "-" + fact.prettyStr(this)
        case Mul(factors) => {
          var sRaw = ""
          for (f <- factors) {
            f match {
              case Pow(base, Num(-1)) => sRaw += " / " + base.prettyStr(this)
              case fact               => sRaw += " * " + fact.prettyStr(this)
            }
          }
          if (sRaw.startsWith(" * ")) sRaw.substring(3)
          else if (sRaw.startsWith(" / ")) "1 / " + sRaw.substring(3)
          else throw new Exception("Internal Error!")
        }
      } 
      Expr.putParentheses(sRep, this, outerNode)
    }

    /** Simplify a n-ary multiplication */
    override def simplify(): Expr = {
      //flatten nested Mul
      val mulF = this.flatten()

      // 0 * a = 0
      if (mulF.factors.contains(Num(0))) return Num(0)

      //multiply the numbers with each other, keep all other elements unchanged
      val (nums, others) = mulF.factors.partition(t => t.isInstanceOf[Num])
      val prod = nums.map(x => x.asInstanceOf[Num].num)
                 .reduceOption((x, y) => x * y)
                 .filterNot(t => t == 1) //if result is `1` remove it
                 .map(Num).toList
      val factsNew = prod ::: others

      //The only remaining factor was a `1` which was filtered out. 
      if (factsNew.length == 0) return Num(1)
      //Remove Muls with only one argument:  (* 23) -> 23
      else if (factsNew.length == 1) factsNew(0)
      else Mul(factsNew)
    }
    
    /** Evaluate n-ary multiplication. */
    override def eval(env: Environment = Environment()): Expr =
      Mul(factors.map(t => t.eval(env))).simplify()
    
    /** 
     * Differentiate n-ary multiplication.
     * 
     * D(u*v*w) = Du*v*w + u*Dv*w + u*v*Dw*/
    override def diff(x: Sym, env: Environment = Environment()): Expr = {
      val summsNew = new ListBuffer[Expr]
      for (i <- 0 until factors.length) {
        val factsNew = ListBuffer.concat(factors)
        factsNew(i) = factsNew(i).diff(x, env)
        summsNew += Mul(factsNew.toList).simplify()
      }
      Add(summsNew.toList).simplify()
    }
  }
  
  /** Power (exponentiation) operator */
  case class Pow(base: Expr, exponent: Expr) extends Expr {
    import Expr.{int2Num, double2Num}
    
    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = {
      val sRep = base.prettyStr(this) + " ~^ " + exponent.prettyStr(this)
      Expr.putParentheses(sRep, this, outerNode)
    }
  
    /** Simplify power. */
    override def simplify(): Expr = {
      this match {
        // a**0 = 1
        case Pow(_, Num(0))                  => Num(1)
        // a**1 = a
        case Pow(base, Num(1))               => base
        // 1**a = 1
        case Pow(Num(1), _)                  => Num(1)
        // Power is inverse of logarithm - can't find general case
        // a ** Log(a, x) = x
        case Pow(pb, Log(lb, x)) if pb == lb => x
        //Two numbers: compute result numerically
        case Pow(Num(base), Num(expo))       => Num(pow(base, expo))
        case _                               => this
      }
    }
    
    /** Evaluate power. */
    override def eval(env: Environment = Environment()): Expr = 
      Pow(base.eval(env), exponent.eval(env)).simplify()
  
    /** Differentiate power. */
    override def diff(x: Sym, env: Environment = Environment()): Expr = {
      this match {
        // Simple case: diff(x**n, x) = n * x**(n-1)
        case Pow(base, Num(expo)) if base == x =>
          expo * (base ~^ (expo-1)).simplify()
        //General case (from Maple):
        //      diff(u(x)~^v(x), x) =
        //        u(x)~^v(x) * (diff(v(x),x)*ln(u(x))+v(x)*diff(u(x),x)/u(x))
        case Pow(u, v) =>
          ((u~^v) * (v.diff(x, env)*Log(E, u) + v*u.diff(x, env)/u)).eval() //eval to simplify
      }
    }
  }
  
  /** Logarithm to arbitrary base */
  case class Log(base: Expr, power: Expr) extends Expr {
    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = 
      "log(" + base.prettyStr(this) + ", " + power.prettyStr(this) + ")"
  
    /** Simplify Logarithms */
    override def simplify(): Expr = {
      this match {
        //log(a, 1) = 0
        case Log(_, Num(1))      => Num(0)
        //log(a, a) = 1
        case Log(b, p) if b == p => Num(1)
        //log(x~^n) = n log(x)
        case Log(b, Pow(x, n))  => n * Log(b, x)
        //Numeric case
        case Log(Num(b), Num(p)) => Num(log(p) / log(b))
        case _ => this
      }
    }
    
    /** Evaluate logarithm. */
    override def eval(env: Environment = Environment()): Expr = 
      Log(base.eval(env), power.eval(env)).simplify()
        
    //TODO: Differentiate logarithms
  }
  
  /**
   * ML style binding operator
   * 
   * Add one binding (name = value) to the environment and evaluate expression
   * `exprNext` in the new environment.
   */
  case class Let(name: String, value: Expr, exprNext: Expr) extends Expr{
    /** Convert instance to a pretty printed string. */
    override def prettyStr(outerNode: Expr = Let("", 0, 0)) = 
      "let " + name + " := " + value.prettyStr(this) + " in \n" + 
      exprNext.prettyStr(this)
    /** Returns this object unchanged. */
    override def simplify() = this
  
    /**
     * Evaluate `let` expression.
     *
     * Add one binding to the environment,
     * and evaluate the next expression in the new environment.
     */
    override def eval(env: Environment = Environment()): Expr = {
      val envNew = env.updated(name, value.eval(env))
      exprNext.eval(envNew)
    }
  
    /** Differentiate `let name = value in exprNext`. */
    override def diff(x: Sym, env: Environment = Environment()): Expr = {
      //Differentiate the value in the original environment.
      val valueD = value.diff(x, env) 
      //Create new environment where derived value has standardized name.
      val valueDName = name + "$" + x.name
      val newEnv =  env.updated(valueDName, valueD)
      //Derive the next expression in the new environment.
      val nextExprD = exprNext.diff(x, newEnv)
      //create the two intertwined let expressions
      val innerLet = Let(valueDName, valueD, nextExprD)
      Let(name, value, innerLet)
      //TODO: simplify let: remove unused variables.
    }
  }  
  
  /** Assignment: `x := a + b`. Used by `let` and `Env` convenience objects. */ 
  case class Asg(lhs: Expr, rhs: Expr) extends Expr
  
  
  /** Type of the environment, contains variables that are assigned by let. */
  type Environment = Map[String, Expr]
  val Environment = Map[String, Expr] _
  
  
  //--- Nicer syntax (the "DSL") ---------------------------------------------
  /** 
   * Convenience object to create an environment from several assignments.
   * 
   * Usage:
   * {{{
   * val (a, b, x) = (Sym("a"), Sym("b"), Sym("x"))
   * val e = Env(a := 2, b := x + 3)
   * }}} */
  object Env {
    import scala.collection.mutable.HashMap
    
    def apply(asgs: Asg*) = {
      val m = new HashMap[String, Expr]()
      
      for (a <- asgs) {
        a match {
          case Asg(Sym(name), rhs) => m(name) = rhs
          case Asg(lhs, rhs) => 
            val msg = "Left hand side of assignment must be a symbol! Got: " +
                      lhs.toString
            throw new Exception(msg)
        }
      }
      m.toMap
    }
  }


  /** Helper object to create (potentially nested) `Let` nodes. 
   *
   * The object accepts multiple assignments. It creates nested `Let` nodes 
   * for multiple assignments. Use like this:
   *    `let (x := 2)` or `let (x := 2, a := 3)`
   * 
   * The object returns a `LetHelper`, that has a method named `in`. 
   *    
   * `let (x := 2)` calls `let.apply(x := 2)`
   * */
  object let {
    def apply(assignments: Asg*) = {
      new LetHelper(assignments.toList)
    }
  }
  
  /** Helper object that embodies the `in` part of (potentially nested) 
   * `let` expressions. 
   * 
   * The `in` method can be called without using a dot or parenthesis.
   * */
  class LetHelper (assignments: List[Asg]) {
    def in(nextExpr: Expr) = {
      //Recursive function that does the real work. Create a `Let` node for  
      //each assignment.
      def makeNestedLets(asgList: List[Asg]): Let = {
        asgList match {
          //End of list, or list has only one element.
          case Asg(Sym(name), value) :: Nil =>      Let(name, value, nextExpr)
          //List has multiple elements. The `let` expression for the remaining 
          //elements is the next expression of the current `let` expression.
          case Asg(Sym(name), value) :: moreAsgs => Let(name, value, makeNestedLets(moreAsgs))
          case _ => throw new Exception("Let expression: assignment required!")
        }
      }
      makeNestedLets(assignments)      
    }
  }
}

/** 
 * Operations on the AST 
 * 
 * This object contains high level wrapper functions that provide a convenient
 * interface to the functionality of `Expr` and its sub-classes.
 * 
 * == Pretty Printing ==
 * 
 * `pprintln`: print the instance in pretty printed form, append ";;".
 * 
 * == Simplification ==
 * 
 * There are numerous simplification routines in this object, but there is
 * no specialized high level interface for simplification. Use `eval`
 * instead.
 * 
 * == Differentiation == 
 * 
 * `diff`: Differentiate this object symbolically. 
 *  
 * == Evaluation == 
 *  
 * `eval`: Compute the value of a numerical (sub) expression, and substitute 
 * known values. performs the usual arithmetic operations. This function
 * performs all implemented simplifications.
 * Terms with unknown symbols are returned un-evaluated. 
 * 
 * The known values (the environment) are given in a `Map(name -> expression)`. 
 * */
object ExprOps {
  import Expression._
 
  /** Print expression in pretty printed (human readable) form. */
  def pprintln(expr: Expr, debug:Boolean = false) {
    expr.pprintln(debug)
  }
  
  /** Compute the derivative symbolically */
  def diff(term: Expr, x: Sym, env: Environment = Environment()): Expr = 
    term.diff(x, env)
           
  /** 
   * Evaluate an expression in an environment where some symbols are known
   * Looks up known symbols, performs the usual arithmetic operations.
   * Terms with unknown symbols are returned un-evaluated. */
  def eval(term: Expr, env: Environment = Environment()): Expr = term.eval(env)
}


/** Test the symbolic math library */
object SymbolicMainOo {
  import Expression._
  import ExprOps._
  import Expr.{int2Num, double2Num}

  //Create some symbols for the tests (unknown variables)
  val (a, b, x) = (Sym("a"), Sym("b"), Sym("x"))
  
  /** Test operators and `let` DSL */
  def testOperators() {
    //The basic operations are implemented
    assert(a + b == Add(a :: b :: Nil))
    assert(a - b == Add(a :: Mul(Num(-1) :: b :: Nil) :: Nil))
    assert(-a == Mul(Num(-1) :: a :: Nil)) 
    assert(a * b == Mul(a :: b :: Nil))
    assert(a / b == Mul(a :: Pow(b, Num(-1)) :: Nil))
    assert(a ~^ b == Pow(a, b))
    //Mixed operations work
    assert(a + 2 == Add(a :: Num(2) :: Nil))
    assert(a - 2 == Add(a :: (Num(-1) * Num(2)) :: Nil))
    assert(a * 2 == Mul(a :: Num(2) :: Nil))
    assert(a / 2 == Mul(a :: Pow(Num(2), Num(-1)) :: Nil))
    assert(a ~^ 2 == Pow(a, Num(2)))
    //Implicit conversions work
    assert(2 + a == Add(Num(2) :: a :: Nil))
    assert(2 - a == Add(Num(2) :: (Num(-1) * a) :: Nil))
    assert(2 * a == Mul(Num(2) :: a :: Nil))
    assert(2 / a == Mul(Num(2) :: Pow(a, Num(-1)) :: Nil))
    assert(2 ~^ a == Pow(Num(2), a))
    //Nested Add and Mul are flattened
    assert(a + b + x == Add(a :: b :: x :: Nil))
    assert(a * b * x == Mul(a :: b :: x :: Nil))
    //mixed + and - work propperly
    assert(a - b + x == Add(a :: (Num(-1) * b) :: x :: Nil))
    //mixed * and / work propperly
    assert(a / b * x == Mul(a :: Pow(b, Num(-1)) :: x :: Nil))
    //create `Let` nodes
    assert((let(a := 1) in a) == Let("a", 1, a))
    assert((let(a := 1, b := 2) in a) == Let("a", 1, Let("b", 2, a)))
  }

  /** Test pretty printing */
  def testPrettyStr() {
    assert(Num(23).prettyStr() == "23.0")
    assert((-Num(2)).prettyStr() == "-2.0")
    assert(a.prettyStr() == "a")
    assert((a + b).prettyStr() == "a + b")
    assert((a - b).prettyStr() == "a - b")
    assert((-a + b).prettyStr() == "-a + b")
    assert((-a).prettyStr() == "-a")
    assert((a * b).prettyStr() == "a * b")
    assert((a / b).prettyStr() == "a / b")
    assert((a ~^ -1 * b).prettyStr() == "1 / a * b")
    assert((a ~^ b).prettyStr() == "a ~^ b")
    assert(Log(a, b).prettyStr() == "log(a, b)")
    assert(Let("a", 2, a + x).prettyStr() == "let a := 2.0 in \na + x")
    //Parentheses if necessary
    assert((a + b + x).prettyStr() == "a + b + x")
    assert((a * b + x).prettyStr() == "a * b + x")
    assert((a * (b + x)).prettyStr() == "a * (b + x)")
    assert((a ~^ (b + x)).prettyStr() == "a ~^ (b + x)") 
  }

  /** test simplification functions */
  def testSimplify() = {
    //Test `simplify Mul`: correct treatment of `-term` as ((-1) * term)  -----
    // -(2) = -2
    assert((-Num(2)).simplify() == Num(-2))
    // --a = a
    assert((-(-a)).simplify() == a)
    // ----a = a
    assert((-(-(-(-a)))).simplify() == a)
    // ---a = -a
    assert((-(-(-a))).simplify() == -a)
    // -a = -a
    assert((-a).simplify() == -a)

    //Test `simplify Mul` -----------------------------------------------
    // 0*a = 0
    assert((0 * a).simplify() == Num(0))
    // 1*1*1 = 1
    assert((Num(1) * 1 * 1).simplify() == Num(1))
    // 1*a = a
    assert((1 * a).simplify() == a)
    // 1 * 2 * 3 = 6
    assert((Num(1) * 2 * 3).simplify() == Num(6))
    // a * b = a * b
    assert((a * b).simplify() == a * b)

    //Test `simplify Add` -----------------------------------------------
    // 0+0+0 = 0
    assert((Num(0) + 0 + 0).simplify() == Num(0))
    // 0+a = 0
    assert((0 + a).simplify() == a)
    // a + (-3) + 3 = a
    assert((a + Num(-3) + 3).simplify() == a)
    // 0 + 1 + 2 + 3 = 6
    assert((Num(0) + 1 + 2 + 3).simplify() == Num(6))
    // a + b = a + b
    assert((a + b).simplify() == a + b)

    //Test `simplify Pow` -----------------------------------------------
    // a~^0 = 1
    assert((a ~^ 0).simplify() == Num(1))
    // a~^1 = a
    assert((a ~^ 1).simplify() == a)
    // 1~^a = 1
    assert((1 ~^ a).simplify() == Num(1))
    // a ~^ log(a, x) = x
    assert((a ~^ Log(a, x)).simplify() == x)
    //2 ~^ 8 = 256: compute result numerically
    assert(Pow(2, 8).simplify() == Num(256))

    //Test `simplify Log` -----------------------------------------------
    //log(a, 1) = 0
    assert((Log(a, 1)).simplify() == Num(0))
    //log(a, a) = 1
    assert((Log(a, a)).simplify() == Num(1))
    //log(x~^n) = n log(x)
    assert((Log(a, x~^b)).simplify() == b * Log(a, x))
    //log(2, 8) = 3 => 2~^3 = 8
    assert((Log(2, 8)).simplify() == Num(3))
  }


  /** Test differentiation */
  def testDiff() = {
    //diff(2, x) must be 0
    assert(diff(Num(2), x) == Num(0))
    //diff(a, x)  must be 0
    assert(diff(a, x) == Num(0))
    //diff(x, x)  must be 1
    assert(diff(x, x) == Num(1))
    //diff(-x, x) must be -1
    //pprintln(diff(Neg(x), x), true)
    assert(diff(-x, x) == Num(-1))
    //diff(2 + a + x, x) must be 1
    assert(diff(2 + a + x, x) == Num(1))
    //diff(2 * x, x) must be 2
    assert(diff(2 * x, x) == Num(2))
    //diff(2 * a * x, x) must be 2 * a
    assert(diff(2 * a * x, x) == 2 * a)
    //diff(x~^2) must be 2*x
    //pprintln(diff(x~^2, x), true)
    assert(diff(x~^2, x) == 2 * x)
    //x~^2 + x + 2 must be 2*x + 1
//    pprintln(x~^2 + x + 2, true)
//    pprintln(diff(x~^2 + x + 2, x), true)
    assert(diff(x~^2 + x + 2, x) == 1 + 2 * x)
    //diff(x~^a, x) = a * x~^(a-1) - correct but needs more simplification
    //pprintln(diff(x~^a, x), true)
    assert(diff(x~^a, x) == (x~^a) * a * (x~^(-1)))
    //diff(a~^x, x) = a~^x * ln(a)
    assert(diff(a~^x, x) == a~^x * Log(E, a))
    
    //Test environment with known derivatives: 
    //The values of the variables `a$x`, `b$x` can be arbitrary. The derivation
    //algorithm does not look at them. Instead the derivation algorithm looks for 
    //variables that contain a "$" character in their names. 
    //Environment: da/dx = a$x
    //diff(a, x) == a$x
    val (a$x, b$x) = (Sym("a$x"), Sym("b$x"))
    assert(diff(a, x, Env(a$x := 0)) == a$x)
    //Environment: da/dx = a$x, db/dx = b$x
    // diff(a * b, x) == a$x * b + a * b$x
    val env1 = Env(a$x := 0, b$x := 0)
    assert(diff(a * b, x, env1) == a$x * b + a * b$x)
    
    //Differentiate `Let` node
    //diff(let a = x~^2 in a + x + 2, x) == let da/dx = 2*x in da/dx + 1 == 2*x + 1
//    pprintln(Let("a", x~^2, a + x + 2), true)
//    pprintln(diff(Let("a", x~^2, a + x + 2), x), true)
    assert(diff(Let("a", x~^2, a + x + 2), x) == 
                Let("a", x~^2,
                Let("a$x", 2 * x, 1 + a$x))) 
    //same as above with `let` DSL
    assert(diff(let(a := x~^2) in a + x + 2, x) == 
           (let(a := x~^2, a$x := 2 * x) in 1 + a$x))
  }


  /** Test evaluation of expressions */
  def testEval() = {
    //Environment: x = 5
    val env = Env(x := 5)
    assert(env == Map("x" -> Num(5)))
    
    // 2 must be 2
    assert(eval(Num(2), env) == Num(2))
    // x must be 5
    assert(eval(x, env) == Num(5))
    // -x must be -5
    assert(eval(-x, env) == Num(-5))
    // -a must be -a
    assert(eval(-a, env) == -a)
    // a - 3 + 3 = a
    assert(eval(a - 3 + 3) == a)
    // x~^2 must be 25
    assert(eval(x~^2, env) == Num(25))
    // x~^a must be 5~^a
    //pprintln(eval(Pow(x, a), env), true)
    assert(eval(x~^a, env) == 5~^a)
    //log(2, 8) must be 3
    assert(eval(Log(2, 8), env) == Num(3))
    // 2 + x + a + 3 must be 10 + a
    //pprintln(eval(Add(Num(2) :: x :: a :: Num(3) :: Nil), env), true)
    assert(eval(2 + x + a + 3, env) == 10 + a)
    // 2 * x * a * 3 must be 30 * a
    assert(eval(2 * x * a * 3, env) == 30 * a)
    //let a = 2 in a + x; must be 7
    //pprintln(Let("a", DNum(2), Add(a :: x :: Nil)), true)
    assert(eval(let(a := 2) in a + x, env) == Num(7))
    //let a = 2 in
    //let b = a * x in
    //let a = 5 in //a is rebound
    //a + b
    // must be 5 + 2 * x
//    pprintln(let(a := 2, b := a * x, a := 5) in a + b)
    assert(eval(let(a := 2, b := a * x, a := 5) in a + b) == 5 + 2 * x)
  }
  
  /** Run the test application. */
  def main(args : Array[String]) : Unit = {
    testOperators()
    testPrettyStr()
    testSimplify()
    testDiff()
    testEval()

    println("Tests finished successfully. (OO)")
  }
}
