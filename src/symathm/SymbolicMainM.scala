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
 * This implementation uses '''pattern matching''', and techniques from 
 * functional programming.
 * 
 * A mathematical expression consists of a recursive tree of Nodes.
 * Nodes contain '''only data''' and have '''no methods'''. The algorithms that 
 * operate on the data (for example the differentiation algorithm) are entirely 
 * separate, and implemented in object `ExprOps`. 
 * 
 * The algorithms use pattern matching to react specifically for different 
 * kinds of nodes.
 */
package symathm

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
   * Implement binary operations for the elements of the AST.
   * `Int` and `Double` can be mixed with `Expr` nodes when using binary 
   * operators, because the companion object defines implicit conversions to 
   * [[symathm.Expression.Num]].
   * 
   * In the following code snippet `myExpr` is an [[symathm.Expression.Add]]. 
   * {{{
   * val x = Sym("x")
   * val myExpr = 2 * x~^2 + 2 * x + 3
   * }}}
   */
  abstract class Expr {
    import ExprOps._
    
    //Binary operators
    def +(other: Expr) = flattenAdd(Add(this :: other :: Nil))
    def -(other: Expr) = Add(this :: other.unary_- :: Nil)
    def unary_-        = Mul(Num(-1) :: this :: Nil)
    def *(other: Expr) = flattenMul(Mul(this :: other :: Nil))
    def /(other: Expr) = Mul(this :: Pow(other, Num(-1)) :: Nil)
    /** Power operator. Can't be `**` or `^`, their precedence is too low. */
    def ~^(other: Expr) = Pow(this, other)
    def :=(other: Expr) = Asg(this, other)
  }

  //The concrete node types
  /** Numbers */
  case class Num(num: Double) extends Expr
  /** Symbols (references to variables) */
  case class Sym(name: String) extends Expr
  /** N-ary addition (+ a b c d). Subtraction is emulated by multiplication with -1. */
  case class Add(summands: List[Expr]) extends Expr
  /** N-ary multiplication (* a b c d); division is emulated with power */
  case class Mul(factors: List[Expr]) extends Expr
  /** Power (exponentiation) operator */
  case class Pow(base: Expr, exponent: Expr) extends Expr
  /** Logarithm to arbitrary base */
  case class Log(base: Expr, power: Expr) extends Expr
  /**
   * ML style binding operator
   * 
   * Add one binding (name = value) to the environment and evaluate expression
   * `exprNext` in the new environment.
   */
  case class Let(name: String, value: Expr, exprNext: Expr) extends Expr  
  /** Assignment: `x := a + b`. Used by `let` and `Env` convenience objects. */ 
  case class Asg(lhs: Expr, rhs: Expr) extends Expr
  
    
  /** Type of the environment, contains variables that are assigned by let. */
  type Environment = Map[String, Expr] 
  val Environment = Map[String, Expr] _
  
  
  //--- Nicer syntax (the "DSL") ---------------------------------------------
  //Implicit conversions so that numbers can be used with the binary operators
  implicit def int2Num(inum: Int) = Num(inum)
  implicit def double2Num(dnum: Double) = Num(dnum)

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


//--- Mathematical operations -------------------------------------------------
/** 
 * Operations on the expression (AST) 
 * 
 * == Pretty Printing ==
 * 
 * `prettyStr`: convert each instance to a pretty printed string.
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
  
  /** 
   * Convert the AST to a string in normal infix notation for math.
   * 
   * @param node      The node that is converted to a string
   * @param outerNode The surrounding node. If this node is a binary operator
   *                  with higher precedence than term, then a pair of 
   *                  parentheses is put around term's string representation.
   * For printing see: [[symathm.ExprOps.pprintln]] */
  def prettyStr(node: Expr, 
                outerNode: Expr = Let("", 0, 0) //`Let` has lowest precedence
                ): String = {
    //Compute precedence of a node, for putting parentheses around the string 
    //representation if necessary.
    def precedence(e: Expr) = e match {
      case Asg(_, _) => 1
      case Add(_)    => 2
      case Mul(_)    => 3
      case Pow(_, _) => 4
      case _         => -1   //No parentheses necessary
    }
    
    val nodeStr = node match {
      case Num(num) => 
        if (num == E) "E"
        else num.toString()
      case Sym(name) => name
      case Add(summands) => {
        var sRaw = ""
        for (s <- summands) {
          s match {
            // (-1) * a => " - a"
            case Mul(Num(-1) :: fact :: Nil) => sRaw += " - " + prettyStr(fact, Mul(Nil))
            case summand                     => sRaw += " + " + prettyStr(summand, node)
          }
        }
        if      (sRaw.startsWith(" + ")) sRaw.substring(3)
        else if (sRaw.startsWith(" - ")) "-" + sRaw.substring(3)
        else throw new Exception("Internal Error!") 
      }
      //convert single "-a": (-1) * a => "-a"
      case Mul(Num(-1) :: fact :: Nil) => "-" + prettyStr(fact, node)
      case Mul(factors)  => {
        var sRaw = ""
        for (f <- factors) {
          f match {
            // a ~^ (-1) => " / a"
            case Pow(base, Num(-1)) => sRaw += " / " + prettyStr(base, Pow(0, 0))
            case fact               => sRaw += " * " + prettyStr(fact, node)
          }
        }
        if      (sRaw.startsWith(" * ")) sRaw.substring(3)
        else if (sRaw.startsWith(" / ")) "1 / " + sRaw.substring(3)
        else throw new Exception("Internal Error!") 
      }
      case Pow(base, exp) => 
        prettyStr(base, node) + " ~^ " + prettyStr(exp, node)
      case Log(base, pow) => 
        "log(" + prettyStr(base, node) + ", " + prettyStr(pow, node) + ")"
      case Let(name, value, in) => 
        "let " + name + " := " + prettyStr(value, node) + " in \n" + 
        prettyStr(in, node)
      case _ => throw new IllegalArgumentException(
                            "Unknown expression: '%s'.".format(node))
    }
    
    //Put parentheses around the term if necessary
    val (precTerm, precOuter) = (precedence(node), precedence(outerNode))
    if (precTerm == -1 || precOuter == -1)  nodeStr //-1: parens are unnecessary
    else if (precTerm < precOuter)          "(" + nodeStr + ")"
    else                                    nodeStr
  }
  
  /** Print AST in human readable form. */
  def pprintln(term: Expr, debug: Boolean = false) = {
    if (debug) {
      println("--- AST ------------------------------------")
      println(term)
      println("--- Human Readable -------------------------")
      println(prettyStr(term) + ";;")
      println()
    } else {
      println(prettyStr(term) + ";;")
    }
  }

  /**
   *  Evaluates an expression.
   *  
   * Evaluate an expression in an environment where some symbols are known
   * Looks up known symbols, performs the usual arithmetic operations.
   * Terms with unknown symbols are returned un-evaluated. 
   * 
   * TODO: Distribute powers: (a*b*c)~^d -> a~^d * b~^d * c~^d
   * TODO: Distribute products: (a+b+c)*d -> -a + -b + -c
   * 
   * @param term  Term that is evaluated.
   * @param env   The environment where the known variables are stored.
   *              It is a map: variable name -> value, with the type:
   *              `String` -> `Expr`. 
   */
  def eval(term: Expr, env: Environment = Environment()): Expr = {
    term match {
      case Sym(name)       => env.getOrElse(name, term)
      case Add(terms)      => simplifyAdd(Add(terms.map(t => eval(t, env))))
      case Mul(terms)      => simplifyMul(Mul(terms.map(t => eval(t, env))))
      case Pow(base, expo) =>
        simplifyPow(Pow(eval(base, env), eval(expo, env)))
      case Log(base, power) =>
        simplifyLog(Log(eval(base, env), eval(power, env)))
      //Add one binding to the environment,
      //and evaluate the next expression in the new environment
      case Let(name, value, exprNext) => {
        val envNew = env.updated(name, eval(value, env))
        eval(exprNext, envNew)
      }
      case _ => term
    }
  }

  /**
   * Convert nested additions to flat n-ary additions:
   * `(+ a (+ b c)) => (+ a b c)`
   */
  def flattenAdd(expr: Add): Add = {
    val summandsNew = new ListBuffer[Expr]
    for (s <- expr.summands) {
      s match {
        case a: Add => summandsNew ++= flattenAdd(a).summands
        case _      => summandsNew += s
      }
    }
    Add(summandsNew.toList)
  }

  /**
   * Convert nested multiplications to flat n-ary multiplications:
   * `(* a (* b c)) => (* a b c)`
   */
  def flattenMul(expr: Mul): Mul = {
    val factorsNew = new ListBuffer[Expr]
    for (s <- expr.factors) {
      s match {
        case m: Mul => factorsNew ++= flattenMul(m).factors
        case _      => factorsNew += s
      }
    }
    Mul(factorsNew.toList)
  }

  /** Simplify a n-ary addition */
  def simplifyAdd(expr: Add): Expr = {
    //flatten nested Add
    val addF = flattenAdd(expr)

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

  /** Simplify a n-ary multiplication */
  def simplifyMul(expr: Mul): Expr = {
    //flatten nested Mul
    val mulF = flattenMul(expr)

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

  /** Simplify Powers */
  def simplifyPow(expr: Pow): Expr = {
    expr match {
      // a~^0 = 1
      case Pow(_, Num(0))                  => Num(1)
      // a~^1 = a
      case Pow(base, Num(1))               => base
      // 1~^a = 1
      case Pow(Num(1), _)                  => Num(1)
      // Power is inverse of logarithm - can't find general case
      // a ~^ (Log(a, x)) = x
      case Pow(pb, Log(lb, x)) if pb == lb => x
      //Two numbers: compute result numerically
      case Pow(Num(base), Num(expo))       => Num(pow(base, expo))
      case _                               => expr
    }
  }

  /** Simplify Logarithms */
  def simplifyLog(expr: Log): Expr = {
    expr match {
      //log(a, 1) = 0
      case Log(_, Num(1))      => Num(0)
      //log(a, a) = 1
      case Log(b, p) if b == p => Num(1)
      //log(x~^n) = n log(x)
      case Log(b, Pow(x, n))  => n * Log(b, x)
      //Numeric case
      case Log(Num(b), Num(p)) => Num(log(p) / log(b))
      case _ => expr
    }
  }

  /** Compute the derivative symbolically */
  def diff(term: Expr, x: Sym, env: Environment = Environment()): Expr = {
    term match {
      case Num(_) => Num(0)
      //The "$" character in variable names denotes derivation: a$x = da/dx
      //This is used for deriving `Let` nodes.
      case Sym(name) => {
        val dName = name + "$" + x.name
        if      (name == x.name)      Num(1)
        else if (env.contains(dName)) Sym(dName)
        else                          Num(0)
      }
      case Add(summands) => simplifyAdd(Add(summands.map(t => diff(t, x, env))))
      //D(u*v*w) = Du*v*w + u*Dv*w + u*v*Dw
      case Mul(factors) =>
        val summands = new ListBuffer[Expr]
        for (i <- 0 until factors.length) {
          val factsNew = ListBuffer.concat(factors)
          factsNew(i) = diff(factsNew(i), x, env)
          summands += simplifyMul(Mul(factsNew.toList))
        }
        simplifyAdd(Add(summands.toList))
      // Simple case: diff(x~^n, x) = n * x~^(n-1)
      case Pow(base, Num(expo)) if base == x =>
        expo * simplifyPow(base ~^ (expo-1))
      //General case (from Maple):
      //      diff(u(x)~^v(x), x) =
      //        u(x)~^v(x) * (diff(v(x),x)*ln(u(x)) + diff(u(x),x)*v(x)/u(x))
      case Pow(u, v) =>
        eval((u~^v) * (diff(v, x, env)*Log(E, u) + diff(u, x, env)*v/u), 
             Environment()) //eval for simplification
      //TODO: Differentiate logarithms
      //Differentiate `let name = value in nextExpr`. 
      case Let(name, value, nextExpr) => {
        //Differentiate the value in the original environment.
        val valueD = diff(value, x, env) 
        //Create new environment where derived value has standardized name.
        val valueDName = name + "$" + x.name
        val newEnv =  env.updated(valueDName, valueD)
        //Derive the next expression in the new environment.
        val nextExprD = diff(nextExpr, x, newEnv)
        //create the two intertwined let expressions
        val innerLet = Let(valueDName, valueD, nextExprD)
        Let(name, value, innerLet)
        //TODO: simplify let: remove unused variables.
      }
    }
  }
}


//--- Tests -------------------------------------------------
/** Test the symbolic maths library */
object SymbolicMainM {
  import Expression._
  import ExprOps._

  //Create some symbols for the tests (unknown variables)
  val (a, b, x) = (Sym("a"), Sym("b"), Sym("x"))
  
  /** Test operators and `let` DSL */
  def testOperators() = {
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
    assert(prettyStr(Num(23)) == "23.0")
    assert(prettyStr(-Num(2)) == "-2.0")
    assert(prettyStr(a) == "a")
    assert(prettyStr(a + b) == "a + b")
    assert(prettyStr(a - b) == "a - b")
    assert(prettyStr(-a + b) == "-a + b")
    assert(prettyStr(-a) == "-a")
    assert(prettyStr(a * b) == "a * b")
    assert(prettyStr(a / b) == "a / b")
    assert(prettyStr(a ~^ -1 * b) == "1 / a * b")
    assert(prettyStr(a ~^ b) == "a ~^ b")
    assert(prettyStr(Log(a, b)) == "log(a, b)")
    assert(prettyStr(Let("a", 2, a + x)) == "let a := 2.0 in \na + x")
    //Parentheses if necessary
    assert(prettyStr(a + b + x) == "a + b + x")
    assert(prettyStr(a * b + x) == "a * b + x")
    assert(prettyStr(a * (b + x)) == "a * (b + x)")
    assert(prettyStr(a ~^ (b + x)) == "a ~^ (b + x)") 
    assert(prettyStr(a + b - (2 + x)) == "a + b - (2.0 + x)")
    assert(prettyStr(a * b / (2 * x)) == "a * b / (2.0 * x)")
  }


  /** test simplification functions */
  def testSimplify() = {
    //Test `simplifyMul`: correct treatment of `-term` as ((-1) * term)  -----
    // -(2) = -2
    assert(simplifyMul(-Num(2)) == Num(-2))
    // --a = a
    assert(simplifyMul(-(-a)) == a)
    // ----a = a
    assert(simplifyMul(-(-(-(-a)))) == a)
    // ---a = -a
    assert(simplifyMul(-(-(-a))) == -a)
    // -a = -a
    assert(simplifyMul(-a) == -a)

    //Test `simplifyMul` -----------------------------------------------
    // 0*a = 0
    assert(simplifyMul(0 * a) == Num(0))
    // 1*1*1 = 1
    assert(simplifyMul(Num(1) * 1 * 1) == Num(1))
    // 1*a = a
    assert(simplifyMul(1 * a) == a)
    // 1 * 2 * 3 = 6
    assert(simplifyMul(Num(1) * 2 * 3) == Num(6))
    // a * b = a * b
    assert(simplifyMul(a * b) == a * b)

    //Test `simplifyAdd` -----------------------------------------------
    // 0+0+0 = 0
    assert(simplifyAdd(Num(0) + 0 + 0) == Num(0))
    // 0+a = 0
    assert(simplifyAdd(0 + a) == a)
    // a + (-3) + 3 = a
    assert(simplifyAdd(a + Num(-3) + 3) == a)
    // 0 + 1 + 2 + 3 = 6
    assert(simplifyAdd(Num(0) + 1 + 2 + 3) == Num(6))
    // a + b = a + b
    assert(simplifyAdd(a + b) == a + b)

    //Test `simplifyPow` -----------------------------------------------
    // a~^0 = 1
    assert(simplifyPow(a ~^ 0) == Num(1))
    // a~^1 = a
    assert(simplifyPow(a ~^ 1) == a)
    // 1~^a = 1
    assert(simplifyPow(1 ~^ a) == Num(1))
    // a ~^ log(a, x) = x
    assert(simplifyPow(a ~^ Log(a, x)) == x)
    //2 ~^ 8 = 256: compute result numerically
    assert(simplifyPow(Pow(2, 8)) == Num(256))

    //Test `simplifyLog` -----------------------------------------------
    //log(a, 1) = 0
    assert(simplifyLog(Log(a, 1)) == Num(0))
    //log(a, a) = 1
    assert(simplifyLog(Log(a, a)) == Num(1))
    //log(x~^n) = n log(x)
    assert(simplifyLog(Log(a, x~^b)) == b * Log(a, x))
    //log(2, 8) = 3 => 2~^3 = 8
    assert(simplifyLog(Log(2, 8)) == Num(3))
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
           
    //Degenerate cases: Mul and Add with no operands.
    //Add(Nil) is equivalent to Num(0)
//   pprintln(diff(Add(Nil), x))
    assert(eval(Add(Nil)) == Num(0))
    assert(diff(Add(Nil), x) == Num(0))
    //Mul(Nil) is equivalent to Num(1)
//   pprintln(diff(Mul(Nil), x))
    assert(eval(Mul(Nil)) == Num(1))
    assert(diff(Mul(Nil), x) == Num(0))
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
    
    //Degenerate cases: Mul and Add with no operands.
    assert(eval(Add(Nil)) == Num(0))
    assert(eval(Mul(Nil)) == Num(1))
  }

  /** Run the test application. */
  def main(args : Array[String]) : Unit = {
    testOperators()
    testPrettyStr()
    testSimplify()
    testDiff()
    testEval()

    println("Tests finished successfully. (M)")
  }
}
