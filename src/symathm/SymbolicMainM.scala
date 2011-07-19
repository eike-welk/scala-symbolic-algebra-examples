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
 * This implementation uses '''pattern matching''',  classes contain 
 * '''only data'''. The algorithms that operate on the data are entirely separate 
 * and reside on object `AstOps`.
 *
 * A much more involved project for a miniature programming language in
 * Scala is Kiama:
 * http://code.google.com/p/kiama/wiki/Lambda2
 *
 * See also this discussion:
 * http://www.scala-lang.org/node/6860
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
    def +(other: Expr) = flatten_add(Add(this :: other :: Nil))
    def -(other: Expr) = Add(this :: other.unary_- :: Nil)
    def unary_-        = Mul(Num(-1) :: this :: Nil)
    def *(other: Expr) = flatten_mul(Mul(this :: other :: Nil))
    def /(other: Expr) = Mul(this :: Pow(other, Num(-1)) :: Nil)
    /** Power operator. Can't be `**` or `^`, their precedence is too low. */
    def ~^(other: Expr) = Pow(this, other)
    def :=(other: Expr) = Asg(this, other)
  }
  object Expr {
    //implicit conversions so that numbers can be used with the binary operators
    implicit def int2Num(inum: Int) = Num(inum)
    implicit def double2Num(dnum: Double) = Num(dnum)
  }
  
  
  //The concrete node types
  /** Numbers */
  case class Num(num: Double) extends Expr
  /** Symbols (references to variables) */
  case class Sym(name: String) extends Expr
  /** N-ary addition (+ a b c d). Subtraction is emulated with the unary minus operator */
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
   * `expr_next` in the new environment.
   */
  case class Let(name: String, value: Expr, exprNext: Expr) extends Expr  
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
  import Expr.{int2Num, double2Num}
  
  /** 
   * Convert the AST to a traditional infix notation for math (String) 
   * 
   * For printing see: [[pprintln]] */
  def prettyStr(term: Expr): String = {
    //TODO: insert braces in the right places
    //TODO: convert `a * b~^-1` to "a / b"
    //TODO: convert `(-1) * a`  to "-a"
    
    //Convert elements of `terms` to strings,
    //and place string `sep` between them
    def convert_join(sep: String, terms: List[Expr]) = {
      val str_lst = terms.map(prettyStr)
      str_lst.reduce((s1, s2) => s1 + sep + s2)
    }

    term match {
      case Num(num) => 
        if (num == E) "E"
        else num.toString()
      case Sym(name)      => name
      case Add(term_lst)  => convert_join(" + ", term_lst)
      case Mul(term_lst)  => convert_join(" * ", term_lst)
      case Pow(base, exp) => prettyStr(base) + " ~^ " + prettyStr(exp)
      case Log(base, pow) => "log(" + prettyStr(base) + ", " + prettyStr(pow) + ")"
      case Let(name, value, in) => 
        "let " + name + " := " + prettyStr(value) + " in \n" + prettyStr(in)
      case _ => throw new IllegalArgumentException(
                  "Unknown expression: '%s'.".format(term))
    }
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
   * @param term  Term that is evaluated.
   * @param env   The environment where the known variables are stored.
   *              It is a map: variable name -> value, with the type:
   *              `String` -> `Expr`. 
   */
  def eval(term: Expr, env: Environment = Environment()): Expr = {
    term match {
      case Sym(name)       => env.getOrElse(name, term)
      case Add(terms)      => simplify_add(Add(terms.map(t => eval(t, env))))
      case Mul(terms)      => simplify_mul(Mul(terms.map(t => eval(t, env))))
      case Pow(base, expo) =>
        simplify_pow(Pow(eval(base, env), eval(expo, env)))
      case Log(base, power) =>
        simplify_log(Log(eval(base, env), eval(power, env)))
      //Add one binding to the environment,
      //and evaluate the next expression in the new environment
      case Let(name, value, expr_next) => {
        val env_new = env.updated(name, eval(value, env))
        eval(expr_next, env_new)
      }
      case _ => term
    }
  }

  /**
   * Convert nested additions to flat n-ary additions:
   * `(+ a (+ b c)) => (+ a b c)`
   */
  def flatten_add(expr: Add): Add = {
    val summands_new = new ListBuffer[Expr]
    for (s <- expr.summands) {
      s match {
        case a: Add => summands_new ++= flatten_add(a).summands
        case _      => summands_new += s
      }
    }
    Add(summands_new.toList)
  }

  /**
   * Convert nested multiplications to flat n-ary multiplications:
   * `(* a (* b c)) => (* a b c)`
   */
  def flatten_mul(expr: Mul): Mul = {
    val factors_new = new ListBuffer[Expr]
    for (s <- expr.factors) {
      s match {
        case m: Mul => factors_new ++= flatten_mul(m).factors
        case _      => factors_new += s
      }
    }
    Mul(factors_new.toList)
  }

  /** Simplify a n-ary addition */
  def simplify_add(expr: Add): Expr = {
    //flatten nested Add
    val add_f = flatten_add(expr)

    // 0 + a = a - remove all "0" elements
    val summands0 = add_f.summands.filterNot(t => t == Num(0))
    if (summands0 == Nil) return Num(0)
    //TODO: Distribute negative sign: -(a+b+c) -> -a + -b + -c

    //sum the numbers up, keep all other elements unchanged
    val (nums, others) = summands0.partition(t => t.isInstanceOf[Num])
    val sum = nums.map(x => x.asInstanceOf[Num].num)
                  .reduceOption((x, y) => x + y).map(Num).toList
    val summands_s = sum ::: others

    //Remove Muls with only one argument:  (* 23) -> 23
    if (summands_s.length == 1) summands_s(0)
    else Add(summands_s)
  }

  /** Simplify a n-ary multiplication */
  def simplify_mul(expr: Mul): Expr = {
    //flatten nested Mul
    val mul_f = flatten_mul(expr)

    // 0 * a = 0
    if (mul_f.factors.contains(Num(0))) return Num(0)
    //TODO: Distribute powers: (a*b*c)~^d -> a~^d * b~^d * c~^d

    //multiply the numbers with each other, keep all other elements unchanged
    val (nums, others) = mul_f.factors.partition(t => t.isInstanceOf[Num])
    val prod = nums.map(x => x.asInstanceOf[Num].num)
                   .reduceOption((x, y) => x * y)
                   .filterNot(t => t == 1) //if result is `1` remove it
                   .map(Num).toList
    val factors_p = prod ::: others

    //The only remaining factor was a `1` which was filtered out. 
    if (factors_p.length == 0) return Num(1)
    //Remove Muls with only one argument:  (* 23) -> 23
    else if (factors_p.length == 1) factors_p(0)
    else Mul(factors_p)
  }

  /** Simplify Powers */
  def simplify_pow(expr: Pow): Expr = {
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
  def simplify_log(expr: Log): Expr = {
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
      case Add(summands) => simplify_add(Add(summands.map(t => diff(t, x, env))))
      //D(u*v*w) = Du*v*w + u*Dv*w + u*v*Dw
      case Mul(factors) =>
        val summands = new ListBuffer[Expr]
        for (i <- 0 until factors.length) {
          val facts_new = ListBuffer.concat(factors)
          facts_new(i) = diff(facts_new(i), x, env)
          summands += simplify_mul(Mul(facts_new.toList))
        }
        simplify_add(Add(summands.toList))
      // Simple case: diff(x~^n, x) = n * x~^(n-1)
      case Pow(base, Num(expo)) if base == x =>
        expo * simplify_pow(base ~^ (expo-1))
      //General case (from Maple):
      //      diff(u(x)~^v(x), x) =
      //        u(x)~^v(x) * (diff(v(x),x)*ln(u(x))+v(x)*diff(u(x),x)/u(x))
      case Pow(u, v) =>
        eval((u~^v) * (diff(v, x, env)*Log(E, u) + v*diff(u, x)/u), Environment()) //eval to simplify
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
        //TODO: simplify_let: remove unused variables.
      }
    }
  }
}


//--- Tests -------------------------------------------------
/** Test the symbolic maths library */
object SymbolicMainM {
  import Expression._
  import ExprOps._
  import Expr.{int2Num, double2Num} //enables `2 * x` instead of `Num(2) * x`

  //Create some symbols for the tests (unknown variables)
  val (a, b, x) = (Sym("a"), Sym("b"), Sym("x"))
  
  /** Test operators and `let` DSL */
  def test_operators() = {
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
  def test_prettyStr() {
    assert(prettyStr(Num(23)) == "23.0")
    assert(prettyStr(a) == "a")
    assert(prettyStr((a + b)) == "a + b")
    pprintln(a - b)
//    assert(prettyStr((a - b)) == "a + -b")
    assert(prettyStr((a * b)) == "a * b")
    assert(prettyStr((a / b)) == "a * b ~^ -1.0")
    assert(prettyStr((a ~^ b)) == "a ~^ b")
    assert(prettyStr(Log(a, b)) == "log(a, b)")
    assert(prettyStr(Let("a", 2, a + x)) == "let a := 2.0 in \na + x")
  }


  /** test simplification functions */
  def test_simplify() = {
    //Test `simplify_mul`: correct treatment of `-term` as ((-1) * term)  -----
    // -(2) = -2
    assert(simplify_mul(-Num(2)) == Num(-2))
    // --a = a
    assert(simplify_mul(-(-a)) == a)
    // ----a = a
    assert(simplify_mul(-(-(-(-a)))) == a)
    // ---a = -a
    assert(simplify_mul(-(-(-a))) == -a)
    // -a = -a
    assert(simplify_mul(-a) == -a)

    //Test `simplify_mul` -----------------------------------------------
    // 0*a = 0
    assert(simplify_mul(0 * a) == Num(0))
    // 1*1*1 = 1
    assert(simplify_mul(Num(1) * 1 * 1) == Num(1))
    // 1*a = a
    assert(simplify_mul(1 * a) == a)
    // 1 * 2 * 3 = 6
    assert(simplify_mul(Num(1) * 2 * 3) == Num(6))
    // a * b = a * b
    assert(simplify_mul(a * b) == a * b)

    //Test `simplify_add` -----------------------------------------------
    // 0+0+0 = 0
    assert(simplify_add(Num(0) + 0 + 0) == Num(0))
    // 0+a = 0
    //pprintln(simplify_add(Add(Num(1) :: a :: Nil)), true)
    assert(simplify_add(0 + a) == a)
    // 0 + 1 + 2 + 3 = 6
    assert(simplify_add(Num(0) + 1 + 2 + 3) == Num(6))
    // a * b = a * b
    assert(simplify_add(a + b) == a + b)

    //Test `simplify_pow` -----------------------------------------------
    // a~^0 = 1
    assert(simplify_pow(a ~^ 0) == Num(1))
    // a~^1 = a
    assert(simplify_pow(a ~^ 1) == a)
    // 1~^a = 1
    assert(simplify_pow(1 ~^ a) == Num(1))
    // a ~^ log(a, x) = x
    assert(simplify_pow(a ~^ Log(a, x)) == x)
    //2 ~^ 8 = 256: compute result numerically
    assert(simplify_pow(Pow(2, 8)) == Num(256))

    //Test `simplify_log` -----------------------------------------------
    //log(a, 1) = 0
    assert(simplify_log(Log(a, 1)) == Num(0))
    //log(a, a) = 1
    assert(simplify_log(Log(a, a)) == Num(1))
    //log(x~^n) = n log(x)
    assert(simplify_log(Log(a, x~^b)) == b * Log(a, x))
    //log(2, 8) = 3 => 2~^3 = 8
    assert(simplify_log(Log(2, 8)) == Num(3))
  }


  /** Test differentiation */
  def test_diff() = {
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
  def test_eval() = {
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
    test_operators()
    test_prettyStr()
    test_simplify()
    test_diff()
    test_eval()

    println("Tests finished successfully. (M)")
  }
}

