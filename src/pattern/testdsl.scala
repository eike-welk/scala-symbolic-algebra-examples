package pattern

/** Define an embedded DSL in Scala.
  * 
  * Create a tree of nodes that represents a mathematical expression.
  * An assignment expression similar to Ocaml's `let` expression
  * is supported, and the majority of the odd code is for implementing it. 
  * 
  * The Scala code to create the tree should look like a regular mathematical 
  * expression, as closely as possible. The let statement should resemble 
  * the statement in Ocaml. (`let x = 3 in x**2` which evaluates to `9` in 
  * Ocaml.) 
  * 
  * This tree is similar to an abstract syntax tree (AST).
  */


package testdsl {
  /** Common base class of all expression nodes. 
   * 
   * For implementing binary operators one only needs the methods of the 
   * `Expr` class and its associated companion object.*/
  class Expr{
    //Binary operators, so that a tree of Expr nodes can be constructed 
    //with the normal mathematical operators. 
    //Only assignment, addition, and power are implemented here.
    def :=(other: Expr) = Asg(this, other)
    def + (other: Expr) = Add(this, other)
    def **(other: Expr) = Pow(this, other)
  }
  object Expr {
    //implicit conversions so that numbers can be used with the binary 
    //operators. The function's names can be chosen arbitrarily.
    implicit def int2Num(inum: Int) = Num(inum)
    implicit def double2Num(dnum: Double) = Num(dnum)
  }
  
  //The concrete node classes.
  /** Symbols: x */
  case class Sym(name: String) extends Expr
  /** Numbers: 23 */
  case class Num(number: Double) extends Expr
  /** Addition: a + b */
  case class Add(sum1: Expr, sum2: Expr) extends Expr
  /** Power: a ** b */
  case class Pow(base: Expr, expo: Expr) extends Expr
  /** Assignment: x := a + b */ 
  case class Asg(lhs: Expr, rhs: Expr) extends Expr
  /** Bind value to name, and evaluate next expression in the new environment: 
   * let (x := a + b) in x ** 2 */
  case class Let(name: String, value: Expr, exprNext: Expr) extends Expr

  
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


/** Test the DSL */
object TestDsl {
  def main(args : Array[String]) : Unit = {
    import testdsl._
    import testdsl.Expr.{double2Num, int2Num} //necessary for e3
    
    //create two variables
    val x = new Sym("x")
    val a = new Sym("a")
    
    //mathematical expressions
    val e1 = a + x
    val e2 = a + 2  //implicit converters looked up in `object Expr`.
    val e3 = 2 + a  //`import testdsl.Expr.int2Num` necessary for this line.
    val e4 = a + x**2
    
    //Simple `let` expression
    val l1 = let(x := a + 3 ) in x ** 2
    //two nested `let` expressions. l2 and l3 are equal.
    val l2 = let(x := 3) in (let(a := x + x) in a**2)
    val l3 = let(x := 3, a := x + x) in a**2

    println(e1)
    println(e2)
    println(e3)
    println(e4)
    println(l1)
    println(l2)
    println(l3)
    
    println("Finished")
  }
}
