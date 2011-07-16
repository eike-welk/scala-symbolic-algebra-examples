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
 * This implementation uses the **Visitor pattern**,  classes contain 
 * **mostly data** with some trivial functions that call into the visitor.
 */
package symathv

import scala.math.{ pow, log, E }
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

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
  //Binary operators
  def +(other: Expr) = AstOps.flatten_add(Add(this :: other :: Nil))
  def -(other: Expr) = Add(this :: Neg(other) :: Nil)
  def *(other: Expr) = AstOps.flatten_mul(Mul(this :: other :: Nil))
  def /(other: Expr) = Mul(this :: Pow(other, Num(-1)) :: Nil)
  /** Warning precedence is too low! Precedences of `**` and `*` are equal. */
  def **(other: Expr) = Pow(this, other)
  def :=(other: Expr) = Asg(this, other)
  
  //Infrastructure for visitors 
  /** Call into visitor that computes a string. */
  def acceptStr(v: StrVisitor): String
  /** Call into visitor that computes an expression. */
  def acceptExpr(v: ExprVisitor): Expr
}

//The concrete node types
/** Numbers */
case class Num(num: Double) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitNum(this)
  override def acceptExpr(v: ExprVisitor) = v.visitNum(this)
}
/** Symbols (references to variables) */
case class Sym(name: String) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitSym(this)
  override def acceptExpr(v: ExprVisitor) = v.visitSym(this)
}
/** Unary minus (-x) */
case class Neg(term: Expr) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitNeg(this)
  override def acceptExpr(v: ExprVisitor) = v.visitNeg(this)
}
/** N-ary addition (+ a b c d). Subtraction is emulated with the unary minus operator */
case class Add(summands: List[Expr]) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitAdd(this)
  override def acceptExpr(v: ExprVisitor) = v.visitAdd(this)
}
/** N-ary multiplication (* a b c d); division is emulated with power */
case class Mul(factors: List[Expr]) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitMul(this)
  override def acceptExpr(v: ExprVisitor) = v.visitMul(this)
}
/** Power (exponentiation) operator */
case class Pow(base: Expr, exponent: Expr) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitPow(this)
  override def acceptExpr(v: ExprVisitor) = v.visitPow(this)
}
/** Logarithm to arbitrary base */
case class Log(base: Expr, power: Expr) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitLog(this)
  override def acceptExpr(v: ExprVisitor) = v.visitLog(this)
}
/**
 * ML style binding operator
 * 
 * Add one binding (name = value) to the environment and evaluate expression
 * `expr_next` in the new environment.
 */
case class Let(name: String, value: Expr, exprNext: Expr) extends Expr {
  override def acceptStr(v: StrVisitor) = v.visitLet(this)
  override def acceptExpr(v: ExprVisitor) = v.visitLet(this)
}
/** Assignment: `x := a + b `
 * Only needed by `let` convenience object which creates `Let` nodes with nicer 
 * syntax. */ 
case class Asg(lhs: Expr, rhs: Expr) extends Expr {
  override def acceptStr(v: StrVisitor) = {throw new Exception("Not implemented!")}
  override def acceptExpr(v: ExprVisitor) = {throw new Exception("Not implemented!")}
}


/**
 * Implicit conversions from [[scala.Int]] and [[scala.Double]] to 
 * [[symbolic_maths.Num]].
 */
object Expr {
  //implicit conversions so that numbers can be used with the binary operators
  implicit def toNum(inum: Int) = Num(inum)
  implicit def toNum(dnum: Double) = Num(dnum)
}


//--- Nicer syntax to create `Let` nodes (the "DSL") --------------------------
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


//--- Mathematical operations -------------------------------------------------
/** 
 * Base class of visitors that return strings. 
 * 
 * Users must first create a visitor. 
 * Then call the method `acceptStr(visitor)` of an `Expr` node, 
 * which in turn calls the correct method of the visitor.
 * */
abstract class StrVisitor {
  def visitNum(num: Num): String
  def visitSym(sym: Sym): String
  def visitNeg(neg: Neg): String
  def visitAdd(add: Add): String
  def visitMul(mul: Mul): String
  def visitPow(pow: Pow): String
  def visitLog(log: Log): String
  def visitLet(let: Let): String
}

/**
 * Convert the nested expressions to a traditional infix notation for math
 * (String).
 *
 * For printing see: [[pprintln]]
 *
 * Users must first create a PrettyStrVisitor. 
 * Then call the method `acceptStr(visitor)` of an `Expr` node, 
 * which in turn calls the correct method of the visitor.
 *
 * ==Examples ==
 *
 * This snippet prints "23.0"
 * {{{
 * val v = new PrettyStrVisitor()
 * println(Num(23).acceptStr(v))
 * }}}
 */
class PrettyStrVisitor extends StrVisitor {
  
  //Convert elements of `terms` to strings,
  //and place string `sep` between them
  def convert_join(sep: String, terms: List[Expr]) = {
    val str_lst = terms.map(t => t.acceptStr(this))
    str_lst.reduce((s1, s2) => s1 + sep + s2)
  }

  //The string conversion functions for the different nodes
  def visitNum(n: Num) = {
    if (n.num == E) "E"
    else n.num.toString()
  }
  def visitSym(sym: Sym) = sym.name.toString()
  def visitNeg(neg: Neg) = "-" + neg.term.acceptStr(this)
  def visitAdd(add: Add): String = convert_join(" + ", add.summands)
  def visitMul(mul: Mul): String = convert_join(" * ", mul.factors)
  def visitPow(pow: Pow): String = 
    pow.base.acceptStr(this) + " ** " + pow.exponent.acceptStr(this)
  def visitLog(log: Log): String = 
    "log(" + log.base.acceptStr(this) + ", " + log.power.acceptStr(this) + ")"
  def visitLet(let: Let): String = 
    "let " + let.name + " := " + let.value.acceptStr(this) + " in \n" + 
    let.exprNext.acceptStr(this)
}


/** 
 * Base class of visitors that return expressions (ASTs). 
 * 
 * Users must first create a visitor. 
 * Then call the method `acceptExpr(visitor)` of an `Expr` node, 
 * which in turn calls the correct method of the visitor.
 * */
abstract class ExprVisitor {
  def visitNum(num: Num): Expr
  def visitSym(sym: Sym): Expr
  def visitNeg(neg: Neg): Expr
  def visitAdd(add: Add): Expr
  def visitMul(mul: Mul): Expr
  def visitPow(pow: Pow): Expr
  def visitLog(log: Log): Expr
  def visitLet(let: Let): Expr
}

  
/**
 * Visitor that evaluates an expression.
 *  
 * Evaluate an expression in an environment where some symbols are known
 * Looks up known symbols, performs the usual arithmetic operations.
 * Terms with unknown symbols are returned un-evaluated. 
 * 
 * For conveniently evaluating expressions use: [[symathv.AstOps.eval]]
 * 
 * Users must first create a EvalVisitor. 
 * Then call the method `acceptExpr(visitor)` of an `Expr` node, 
 * which in turn calls the correct method of the visitor.
 *
 * ==Examples ==
 *
 * This snippet prints "23.0;;"
 * {{{
 * val v = new EvalVisitor()
 * pprintln((Num(21) + 2).acceptExpr(v))
 * }}}
 */
class EvalVisitor(inEnvironment: AstOps.Environ) extends ExprVisitor {
  import AstOps._
  
  //Stack of environments, the variables are looked up in the environment at 
  //the top of the stack. The `let` statement pushes its new environment on 
  //this stack, and pops it when it exits. 
  val envStack = Stack(inEnvironment)
  
  /** Get the current environment. */
  def env = envStack.top
  
  def visitNum(num: Num) = num
  def visitSym(sym: Sym) = env.getOrElse(sym.name, sym)
  def visitNeg(neg: Neg) = simplify_neg(Neg(neg.term.acceptExpr(this)))
  def visitAdd(add: Add) = simplify_add(Add(add.summands.map(t => t.acceptExpr(this))))
  def visitMul(mul: Mul) = simplify_mul(Mul(mul.factors.map(t => t.acceptExpr(this))))
  def visitPow(pow: Pow) = 
    simplify_pow(Pow(pow.base.acceptExpr(this), pow.exponent.acceptExpr(this)))
  def visitLog(log: Log) = 
    simplify_log(Log(log.base.acceptExpr(this), log.power.acceptExpr(this)))
  def visitLet(let: Let) = {
    //Add one binding to the environment,
    //and evaluate the next expression in the new environment
    val env_new = env.updated(let.name, let.value.acceptExpr(this))
    envStack.push(env_new)
    val resultNext = let.exprNext.acceptExpr(this)
    envStack.pop()
    resultNext
  }
}


/** 
 * Visitor that computes the derivative of an expression (symbolically). 
 * 
 * For a high level interface look at [[symathv.AstOps.diff]].
 * 
 * Users must first create a DiffVisitor. 
 * Then call the method `acceptExpr(visitor)` of an `Expr` node, 
 * which in turn calls the correct method of the visitor.
 *
 * ==Examples ==
 *
 * This snippet prints "23.0;;"
 * {{{
 * val v = new DiffVisitor()
 * val x = Sym("x")
 * pprintln((Num(23) * x).acceptExpr(v))
 * }}}
 *
 * @param x    The independent variable. Derivation takes place with 
 *             respect to this variable.
 * @param env  The environment where known variables are defined. 
 *             Necessary for computing derivative of `Let` node.
 *             Empty environments are created with: `Environ()`.
 * */
class DiffVisitor(x: Sym, env: AstOps.Environ)  extends ExprVisitor {
  import AstOps._
  
  def visitNum(num: Num): Expr = Num(0)
  
  //The "$" character in variable names denotes derivation: a$x = da/dx
  //This is used for deriving `Let` nodes.
  def visitSym(sym: Sym): Expr = {
    val dName = sym.name + "$" + x.name
    if (sym.name == x.name)       Num(1)
    else if (env.contains(dName)) Sym(dName)
    else                          Num(0)
  }
  
  def visitNeg(neg: Neg): Expr = simplify_neg(Neg(diff(neg.term, x, env)))
  def visitAdd(add: Add): Expr = simplify_add(Add(add.summands.map(t => diff(t, x, env))))
  
  //D(u*v*w) = Du*v*w + u*Dv*w + u*v*Dw
  def visitMul(mul: Mul): Expr = {
    val summands = new ListBuffer[Expr]
    for (i <- 0 until mul.factors.length) {
      val facts_new = ListBuffer.concat(mul.factors)
      facts_new(i) = diff(facts_new(i), x, env)
      summands += simplify_mul(Mul(facts_new.toList))   
    }
    simplify_add(Add(summands.toList))
  }
  
  def visitPow(pow: Pow): Expr = {
    // Simple case: diff(x**n, x) = n * x**(n-1)
    if (pow.base == x && pow.exponent.isInstanceOf[Num]) {
      val expo = pow.exponent.asInstanceOf[Num]
      pow.exponent * simplify_pow(pow.base ** (Num(expo.num-1)))
    }
    //General case (from Maple):
    //      diff(u(x)**v(x), x) =
    //        u(x)**v(x) * (diff(v(x),x)*ln(u(x))+v(x)*diff(u(x),x)/u(x))
    else {
      val (u, v) = (pow.base, pow.exponent)
      eval((u**v) * (diff(v, x, env)*Log(E, u) + v*diff(u, x)/u), Environ()) //eval to simplify 
    }
  }
  
  //TODO: Differentiate logarithms
  def visitLog(log: Log): Expr = {throw new Exception("Not implemented!")}
  
  //Differentiate `let name = value in nextExpr`. 
  def visitLet(let: Let): Expr = {
    val (name, value, nextExpr) = (let.name, let.value, let.exprNext)
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

  
/** 
 * Functions for operations on expressions
 * 
 * Especially useful are the following high level wrapper functions.
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
object AstOps {
  /** Type of the environment, contains variables that are assigned by let. */
  type Environ = Map[String, Expr]
  val Environ = Map[String, Expr] _
  
  /** 
   * Convert the AST to a traditional infix notation for math (String) 
   * 
   * For printing see: [[pprintln]] */
  def prettyStr(e: Expr) = {
    val v = new PrettyStrVisitor()
    e.acceptStr(v)
  }
  
  /** Print AST in human readable form. */
  def pprintln(term: Expr, debug: Boolean = false) = {
    val v = new PrettyStrVisitor()
    
    if (debug) {
      println("--- AST ------------------------------------")
      println(term)
      println("--- Human Readable -------------------------")
      println((term.acceptStr(v)) + ";;")
      println()
    } else {
      println((term.acceptStr(v)) + ";;")
    }
  }

  /**
   *  Evaluate an expression.
   *  
   * Evaluates an expression in an environment where some symbols are known.
   * Looks up known symbols, performs the usual arithmetic operations.
   * Terms with unknown symbols are returned un-evaluated. 
   */
  def eval(term: Expr, env: Environ = Environ()): Expr = {
    val v = new EvalVisitor(env)
    term.acceptExpr(v)
  }
  
  /** Compute the derivative symbolically */
  def diff(term: Expr, x: Sym, env: Environ = Environ()): Expr = {
    val v = new DiffVisitor(x, env)
    term.acceptExpr(v)
  }

  /**
   * Convert nested additions to flat n-ary additions:
   * `(+ a (+ b c)) => (+ a b c)`
   */
  def flatten_add(expr: Add): Add = {
    val summands_new = new ListBuffer[Expr]
    for (s <- expr.summands) {
      if (s.isInstanceOf[Add]) {
        val a = s.asInstanceOf[Add]
        summands_new ++= flatten_add(a).summands
      }
      else {
        summands_new += s
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
      if (s.isInstanceOf[Mul]) {
        val m = s.asInstanceOf[Mul]
        factors_new ++= flatten_mul(m).factors
      }
      else {
        factors_new += s
      }
    }
    Mul(factors_new.toList)
  }

  /** Simplify minus sign (Neg) */
  def simplify_neg(expr: Expr): Expr = {
    if (expr.isInstanceOf[Neg]) {
      val neg = expr.asInstanceOf[Neg]
      //case Neg(Num(num)) => Num(-num)
      if (neg.term.isInstanceOf[Num]) {
        val num = neg.term.asInstanceOf[Num]
        return Num(-num.num)
      //case Neg(Neg(term)) => simplify_neg(term)
      } else if (neg.term.isInstanceOf[Neg]) {
        val negInner = neg.term.asInstanceOf[Neg]
        return simplify_neg(negInner.term)
      }
    }
    //case _ => expr
    return expr
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
    // 1 * a = a - remove all "1" elements
    val factors1 = mul_f.factors.filterNot(t => t == Num(1))
    if (factors1 == Nil) return Num(1)
    //TODO: Distribute powers: (a*b*c)**d -> a**d * b**d * c**d

    //multiply the numbers with each other, keep all other elements unchanged
    val (nums, others) = factors1.partition(t => t.isInstanceOf[Num])
    val prod = nums.map(x => x.asInstanceOf[Num].num)
                   .reduceOption((x, y) => x * y).map(Num).toList
    val factors_p = prod ::: others

    //Remove Muls with only one argument:  (* 23) -> 23
    if (factors_p.length == 1) factors_p(0)
    else Mul(factors_p)
  }

  /** Simplify Powers */
  def simplify_pow(power: Pow): Expr = {
    // a**0 = 1
    if (power.exponent.isInstanceOf[Num] && 
        power.exponent.asInstanceOf[Num].num == 0) {
      return Num(1)
    }
    // a**1 = a
    if (power.exponent.isInstanceOf[Num] && 
        power.exponent.asInstanceOf[Num].num == 1) {
      return power.base
    }
    // 1**a = 1
    if (power.base.isInstanceOf[Num] && 
        power.base.asInstanceOf[Num].num == 1) {
      return Num(1)
    }
    // Power is inverse of logarithm - can't find general case
    // a ** Log(a, x) = x
    if (power.exponent.isInstanceOf[Log]) {
      val log = power.exponent.asInstanceOf[Log]
      if (power.base == log.base) {
        return log.power
      }
    }
    //Two numbers: compute result numerically
    if (power.base.isInstanceOf[Num] && power.exponent.isInstanceOf[Num]) {
      val base = power.base.asInstanceOf[Num].num
      val exp = power.exponent.asInstanceOf[Num].num
      return Num(pow(base, exp))
    }
    
    return power
  }

  /** Simplify Logarithms */
  def simplify_log(logNode: Log): Expr = {
    //log(a, 1) = 0
    if (logNode.power == Num(1)) {
      return Num(0)
    }
    //log(a, a) = 1
    if (logNode.base == logNode.power) {
      return Num(1)
    }
    //log(b, x**n) = n log(b, x)
    if (logNode.power.isInstanceOf[Pow]) {
      val b = logNode.base
      val x = logNode.power.asInstanceOf[Pow].base
      val n = logNode.power.asInstanceOf[Pow].exponent
      return n * Log(b, x)
    }
    //Numeric case
    if (logNode.base.isInstanceOf[Num] && logNode.power.isInstanceOf[Num]) {
      val b = logNode.base.asInstanceOf[Num].num
      val p = logNode.power.asInstanceOf[Num].num
      return Num(log(p) / log(b))
    }
    
    return logNode
  }
}

/** Test the symbolic maths library */
object SymbolicMainV {
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
    //create `Let` nodes
    assert((let(a := 1) in a) == Let("a", 1, a))
    assert((let(a := 1, b := 2) in a) == Let("a", 1, Let("b", 2, a)))
  }


  /** Test pretty printing */
  def test_prettyStr() {
    val v = new PrettyStrVisitor()
    
    assert(prettyStr(Num(23)) == "23.0")
    assert(prettyStr((a)) == "a")
    assert(prettyStr(Neg(2)) == "-2.0")
    assert(prettyStr((a + b)) == "a + b")
    assert(prettyStr((a - b)) == "a + -b")
    assert(prettyStr((a * b)) == "a * b")
    assert(prettyStr((a / b)) == "a * b ** -1.0")
    assert(prettyStr((a ** b)) == "a ** b")
    assert(prettyStr(Log(a, b)) == "log(a, b)")
    assert(prettyStr(Let("a", 2, a + x)) == 
           "let a := 2.0 in \na + x")
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

    //Test `simplify_pow` -----------------------------------------------
    // a**0 = 1
    assert(simplify_pow(a ** 0) == Num(1))
    // a**1 = a
    assert(simplify_pow(a ** 1) == a)
    // 1**a = 1
    assert(simplify_pow(1 ** a) == Num(1))
    // a ** log(a, x) = x
    assert(simplify_pow(a ** Log(a, x)) == x)
    //2 ** 8 = 256: compute result numerically
    assert(simplify_pow(Pow(2, 8)) == Num(256))

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
    
    //Test environment with known derivatives: 
    //The values of the variables `a$x`, `b$x` can be arbitrary. The derivation
    //algorithm does not look at them. Instead the derivation algorithm looks for 
    //variables that contain a "$" character in their names. 
    //Environment: da/dx = a$x
    //diff(a, x) == a$x
    val (a$x, b$x) = (Sym("a$x"), Sym("b$x"))
    assert(diff(a, x, Environ("a$x" -> 0)) == a$x)
    //Environment: da/dx = a$x, db/dx = b$x
    // diff(a * b, x) == a$x * b + a * b$x
    val env1 = Environ("a$x"->0, "b$x"->0)
    assert(diff(a * b, x, env1) == a$x * b + a * b$x)
    
    //Differentiate `Let` node
    //diff(let a = x**2 in a + x + 2, x) == let da/dx = 2*x in da/dx + 1 == 2*x + 1
//    pprintln(Let("a", x**2, a + x + 2), true)
//    pprintln(diff(Let("a", x**2, a + x + 2), x), true)
    assert(diff(Let("a", x**2, a + x + 2), x) == 
                Let("a", x**2,
                Let("a$x", 2 * x, 1 + a$x))) 
    //same as above with `let` DSL
    assert(diff(let(a := x**2) in a + x + 2, x) == 
           (let(a := x**2, a$x := 2 * x) in 1 + a$x))
  }


  /** Test evaluation of expressions */
  def test_eval() = {
    //Environment: x = 5
    val env = Map("x" -> Num(5))
    
    // 2 must be 2
    assert(eval(Num(2), env) == Num(2))
    // x must be 5
    assert(eval(x, env) == Num(5))
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

    println("Tests finished successfully. (V)")
  }
}

