package pattern

/** Define an embedded DSL in Scala.
  *
  */
package testdsl {
  //Common base class of all AST nodes
  class Expr{
    def :=(other: Expr) = Asg(this, other)
    def + (other: Expr) = Add(this, other)
    def **(other: Expr) = Pow(this, other)
  }
  object Expr {
    //implicit conversions so that numbers can be used with the binary operators
    implicit def toNum(inum: Int) = Num(inum)
    implicit def toNum(dnum: Double) = Num(dnum)
  
    def let(equ: Asg)(nextExpr: Expr) = {
      val (name, value) = equ match {
        case Asg(Sym(name), value) => (name, value)
        case _ => throw new Exception("Let expression: no assignment!")
      }
      Let(name, value, nextExpr)
    }
  }
  
  //The concrete node types
  case class Sym(name: String) extends Expr
  case class Num(number: Double) extends Expr
  case class Add(fac1: Expr, fac2: Expr) extends Expr
  case class Pow(base: Expr, expo: Expr) extends Expr
  case class Asg(lhs: Expr, rhs: Expr) extends Expr
  case class Let(name: String, value: Expr, exprNext: Expr) extends Expr
}  

object TestDsl {
  def main(args : Array[String]) : Unit = {
    import testdsl._
    import testdsl.Expr.let
    
    val x = new Sym("x")
    val a = new Sym("a")
    val e1 = let(x := a + 3)(x ** 2)
    val e2 = let(x := 3)(let(a := x + x)(a))
    
    println(e1)
    println(e2)
    println("Finished")
  }
}
