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
 * This implementation uses **pattern matching**,  classes are **pure data**.
 *
 * A much more involved project for a miniature programming language in
 * Scala is Kiama:
 * http://code.google.com/p/kiama/wiki/Lambda2
 *
 * See also this discussion:
 * http://www.scala-lang.org/node/6860
 */
package symbolic_maths

import scala.math.{ pow, log, E }
import scala.collection.mutable.ListBuffer

//The elements of the AST ----------------------------------------------
//Expressions also evaluate to nodes of the AST

/**
 * Common base class of all AST nodes.
 *
 * Implement binary operations for the elements of the AST.
 * `Int` and `Double` can be mixed with `Expr` (AST) nodes when using binary 
 * operators, because the companion object defines implicit conversions to 
 * [[symbolic_maths.Num]].
 * 
 * In the following code snippet `myExpr` is an [[symbolic_maths.Add]]. Note
 * the parenthesis around `x**2`; the power operators precedence is too low. 
 * It is equal to the precedence of `*`.
 * {{{
 * val x = Sym("x")
 * val myExpr = 2 * (x**2) + 2 * x + 3
 * }}}
 */
abstract class Expr {
  import AstOps.{ flatten_add, flatten_mul }

  def +(other: Expr) = flatten_add(Add(this :: other :: Nil))
  def -(other: Expr) = Add(this :: Neg(other) :: Nil)
  def *(other: Expr) = flatten_mul(Mul(this :: other :: Nil))
  def /(other: Expr) = Mul(this :: Pow(other, Num(-1)) :: Nil)
  /** Warning precedence is too low! Precedences of `**` and `*` are equal. */
  def **(other: Expr) = Pow(this, other)
}

//The concrete node types
/** Numbers */
case class Num(num: Double) extends Expr
/** Symbols (references to variables) */
case class Sym(name: String) extends Expr
/** Unary minus (-x) */
case class Neg(term: Expr) extends Expr
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
case class Let(name: String, value: Expr, expr_next: Expr) extends Expr


/**
 * Implicit conversions from [[scala.Int]] and [[scala.Double]] to 
 * [[symbolic_maths.Num]].
 */
object Expr {
  //implicit conversions so that numbers can be used with the binary operators
  implicit def toNum(inum: Int) = Num(inum)
  implicit def toNum(dnum: Double) = Num(dnum)
}


//--- Operations on the AST -------------------------------------------------
object AstOps {
  //Convert the AST to a traditional infix notation for math (String)
  def prettyStr(term: Expr): String = {
    //TODO: insert braces in the right places
    //TODO: convert a * b**-1 to a / b
    
    //Convert elements of `terms` to strings,
    //and place string `sep` between them
    def convert_join(sep: String, terms: List[Expr]) = {
      val str_lst = terms.map(prettyStr)
      str_lst.reduce((s1, s2) => s1 + sep + s2)
    }

    term match {
      case Num(num)       => num match {
        case E => "E"
        case _ => num.toString()
      }
      case Sym(name)      => name
      case Neg(term)      => "-" + prettyStr(term)
      case Add(term_lst)  => convert_join(" + ", term_lst)
      case Mul(term_lst)  => convert_join(" * ", term_lst)
      case Pow(base, exp) => prettyStr(base) + " ** " + prettyStr(exp)
      case Log(base, pow) => "Log(" + prettyStr(base) + ", " + prettyStr(pow) + ")"
      case Let(name, value, in) => 
        "let " + name + " = " + prettyStr(value) + " in \n" + prettyStr(in)
      case _ => throw new IllegalArgumentException(
                  "Unknown expression: '%s'.".format(term))
    }
  }
  
  //print AST in human readable form.
  def pprintln(term: Expr, debug: Boolean = false) = {
    if (debug) {
      println()
      println("--- AST ------------------------------------")
      println(term)
      println("--- Human Readable -------------------------")
      println(prettyStr(term) + ";;")
      println()
    } else {
      println(prettyStr(term) + ";;")
    }
  }

  //Evaluate an expression in an environment where some symbols are known
  //Looks up known symbols, performs the usual arithmetic operations.
  //Terms with unknown symbols are returned un-evaluated.
  def eval(term: Expr, env: Map[String, Expr] = Map[String, Expr]()): Expr = {
    term match {
      case Sym(name)       => env.getOrElse(name, term)
      case Neg(term)       => simplify_neg(Neg(eval(term, env)))
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

  /** Converts a Num to a Double. (Throw exception for any other `Expr` in 
   * `num`.) */
  def num2double(num: Expr) = num match {
    case Num(dbl) => dbl
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
        case x      => summands_new ++= List(x)
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
        case x      => factors_new ++= List(x)
      }
    }
    Mul(factors_new.toList)
  }

  /** Simplify minus sign (Neg) */
  def simplify_neg(expr: Neg): Expr = {
    expr match {
      case Neg(Num(num))         => Num(-num)
      //--a = a
      case Neg(Neg(neg_rec:Neg)) => simplify_neg(neg_rec)
      case Neg(Neg(term))        => term
      case _                     => expr
    }
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
    val sum = nums.map(num2double).reduceOption((x, y) => x + y)
                  .map(Num).toList
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
    // 1 * a = a - remove all "1" elements
    val factors1 = mul_f.factors.filterNot(t => t == Num(1))
    if (factors1 == Nil) return Num(1)
    //TODO: Distribute powers: (a*b*c)**d -> a**d * b**d * c**d

    //multiply the numbers with each other, keep all other elements unchanged
    val (nums, others) = factors1.partition(t => t.isInstanceOf[Num])
    val prod = nums.map(num2double).reduceOption((x, y) => x * y)
                   .map(Num).toList
    val factors_p = prod ::: others

    //Remove Muls with only one argument:  (* 23) -> 23
    if (factors_p.length == 1) factors_p(0)
    else Mul(factors_p)
  }

  /** Simplify Powers */
  def simplify_pow(expr: Pow): Expr = {
    expr match {
      // a**0 = 1
      case Pow(_, Num(0))                  => Num(1)
      // a**1 = a
      case Pow(base, Num(1))               => base
      // 1**a = 1
      case Pow(Num(1), _)                  => Num(1)
      // Power is inverse of logarithm - can't find special case
      case Pow(pb, Log(lb, x)) if pb == lb => x
      //Two numbers: compute result numerically
      case Pow(Num(base), Num(expo))       => Num(pow(base, expo))
      case _                               => expr
    }
  }

  /** Simplify LOgarithms */
  def simplify_log(expr: Log): Expr = {
    expr match {
      //log(a, 1) = 0
      case Log(_, Num(1))      => Num(0)
      //log(a, a) = 1
      case Log(b, p) if b == p => Num(1)
      //log(x**n) = n log(x)
      case Log(b, Pow(x, n))  => n * Log(b, x)
      //Numeric case
      case Log(Num(b), Num(p)) => Num(log(p) / log(b))
      case _ => expr
    }
  }

  /** Compute the derivative symbolically */
  def diff(term: Expr, x: Sym,
           env: Map[String, Expr] = Map[String, Expr]()): Expr = {
    import Expr.toNum

    term match {
      case Num(_)     => Num(0)
      case Sym(_)     => if (term == x) Num(1) else Num(0)
      case Neg(term)  => simplify_neg(Neg(diff(term, x)))
      case Add(summands) => simplify_add(Add(summands.map(t => diff(t, x))))
      //D(u*v*w) = Du*v*w + u*Dv*w + u*v*Dw
      case Mul(factors) =>
        val summands = new ListBuffer[Expr]
        for (i <- 0 until factors.length) {
          val facts_new = ListBuffer.concat(factors)
          facts_new(i) = diff(facts_new(i), x)
          summands += simplify_mul(Mul(facts_new.toList))
        }
        simplify_add(Add(summands.toList))
      // diff(x**n, x) = n * x**(n-1) - The simple middle school case
      case Pow(base, Num(expo)) if base == x =>
        expo * simplify_pow(base ** (expo-1))
      //General case (from Maple):
      //      diff(u(x)**v(x), x) =
      //        u(x)**v(x) * (diff(v(x),x)*ln(u(x))+v(x)*diff(u(x),x)/u(x))
      case Pow(u, v) =>
        eval((u**v) * (diff(v, x)*Log(E, u) + v*diff(u, x)/u))
      //TODO: Differentiate `Let`. 
      //      Let needs environment like `eval`.
      case Let(name, value, nextExpr) => {
        val inExprDiff = Num(-0)
        val valueDiff = Num(-0)
        val innerLet = Let(name + "$" + x.name, valueDiff, inExprDiff)
        Let(name, value, innerLet)
      }
    }
  }
}

//--- Test the symbolic maths library --------------------------------------
object SymbolicMain {
  import AstOps._
  import Expr.toNum

  //Create some symbols for the tests (unknown variables)
  val (a, b, x) = (Sym("a"), Sym("b"), Sym("x"))
  
  /** Test binary operators */
  def test_operators() = {
    //The basic operations are implemented
    assert(a + b == Add(a :: b :: Nil))
    assert(a - b == Add(a :: Neg(b) :: Nil))
    assert(a * b == Mul(a :: b :: Nil))
    assert(a / b == Mul(a :: Pow(b, Num(-1)) :: Nil))
    assert(a ** b == Pow(a, b))
    //Mixed operations work
    assert(a + 2 == Add(a :: Num(2) :: Nil))
    assert(a - 2 == Add(a :: Neg(Num(2)) :: Nil))
    assert(a * 2 == Mul(a :: Num(2) :: Nil))
    assert(a / 2 == Mul(a :: Pow(Num(2), Num(-1)) :: Nil))
    assert(a ** 2 == Pow(a, Num(2)))
    //Implicit conversions work
    assert(2 + a == Add(Num(2) :: a :: Nil))
    assert(2 - a == Add(Num(2) :: Neg(a) :: Nil))
    assert(2 * a == Mul(Num(2) :: a :: Nil))
    assert(2 / a == Mul(Num(2) :: Pow(a, Num(-1)) :: Nil))
    assert(2 ** a == Pow(Num(2), a))
    //Nested Add and Mul are flattened
    assert(a + b + x == Add(a :: b :: x :: Nil))
    assert(a * b * x == Mul(a :: b :: x :: Nil))
    //mixed + and - work propperly
    assert(a - b + x == Add(a :: Neg(b) :: x :: Nil))
    //mixed * and / work propperly
    assert(a / b * x == Mul(a :: Pow(b, Num(-1)) :: x :: Nil))
  }


  /** test simplification functions */
  def test_simplify() = {
    //Test `simplify_neg` -----------------------------------------------
    // -(2) = -2
    assert(simplify_neg(Neg(Num(2))) == Num(-2))
    // --a = a
    assert(simplify_neg(Neg(Neg(a))) == a)
    // ----a = a
    //pprintln(simplify_neg(Neg(Neg(Neg(Neg(a))))), true)
    assert(simplify_neg(Neg(Neg(Neg(Neg(a))))) == a)
    // ---a = -a
    assert(simplify_neg(Neg(Neg(Neg(a)))) == Neg(a))
    // -a = -a
    assert(simplify_neg(Neg(a)) == Neg(a))

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

    //Test `simplify_log` -----------------------------------------------
    //log(a, 1) = 0
    assert(simplify_log(Log(a, 1)) == Num(0))
    //log(a, a) = 1
    assert(simplify_log(Log(a, a)) == Num(1))
    //log(x**n) = n log(x)
    assert(simplify_log(Log(a, x**b)) == b * Log(a, x))
    //log(2, 8) = 3 => 2**3 = 8
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
    assert(diff(Neg(x), x) == Num(-1))
    //diff(2 + a + x, x) must be 1
    assert(diff(2 + a + x, x) == Num(1))
    //diff(2 * x, x) must be 2
    assert(diff(2 * x, x) == Num(2))
    //diff(2 * a * x, x) must be 2 * a
    assert(diff(2 * a * x, x) == 2 * a)
    //diff(x**2) must be 2*x
    //pprintln(diff(x**2, x), true)
    assert(diff(x**2, x) == 2 * x)
    //x**2 + x + 2 must be 2*x + 1
//    pprintln(x**2 + x + 2, true)
//    pprintln(diff(x**2 + x + 2, x), true)
    assert(diff(x**2 + x + 2, x) == 1 + 2 * x)
    //diff(x**a, x) = a * x**(a-1) - correct but needs more simplification
    //pprintln(diff(x**a, x), true)
    assert(diff(x**a, x) == (x**a) * a * (x**(-1)))
    //diff(a**x, x) = a**x * ln(a)
    assert(diff(a**x, x) == a**x * Log(E, a))
    
    //diff(let a = x**2 in 
    //     a + x + 2, x)   =   2*x + 1
    pprintln(Let("a", x**2, a + x + 2), true)
    pprintln(diff(Let("a", x**2, a + x + 2), x), true)
  }


  /** Test evaluation of expressions */
  def test_eval() = {
    //Environment: x = 5
    val env = Map("x" -> Num(5))
    // -x must be -5
    assert(eval(Neg(x), env) == Num(-5))
    // -a must be -a
    assert(eval(Neg(a), env) == Neg(a))
    // x**2 must be 25
    assert(eval(x**2, env) == Num(25))
    // x**a must be 5**a
    //pprintln(eval(Pow(x, a), env), true)
    assert(eval(x**a, env) == 5**a)
    //log(2, 8) must be 3
    assert(eval(Log(2, 8), env) == Num(3))
    // 2 + x + a + 3 must be 10 + a
    //pprintln(eval(Add(Num(2) :: x :: a :: Num(3) :: Nil), env), true)
    assert(eval(2 + x + a + 3, env) == 10 + a)
    // 2 * x * a * 3 must be 30 * a
    assert(eval(2 * x * a * 3, env) == 30 * a)
    //let a = 2 in a + x; must be 7
    //pprintln(Let("a", DNum(2), Add(a :: x :: Nil)), true)
    assert(eval(Let("a", 2, a + x), env) == Num(7))
    //let a = 2 in
    //let b = a * x in
    //let a = 5 in //a is rebound
    //a + b
    // must be 5 + 2 * x
//    pprintln(Let("a", 2, 
//             Let("b", a * x,
//             Let("a", 5, a +b))), true)
    val emptyEnv = Map[String, Expr]()
    assert(eval(Let("a", 2, 
                Let("b", a * x,
                Let("a", 5, a +b))), emptyEnv) == 5 + 2 * x)
  }

  /** Run the test application. */
  def main(args : Array[String]) : Unit = {
    test_operators()
    test_simplify()
    test_diff()
    test_eval()

    println("Tests finished successfully.")
  }
}

