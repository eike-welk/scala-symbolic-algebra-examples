package pattern

/** Define an embedded DSL in Scala.
  *
  */
package testdsl {
  //Common base class of all Expression nodes
  class Expr{
    def :=(other: Expr) = Asg(this, other)
    def + (other: Expr) = Add(this, other)
    def **(other: Expr) = Pow(this, other)
  }
  object Expr {
    //implicit conversions so that numbers can be used with the binary operators
    implicit def toNum(inum: Int) = Num(inum)
    implicit def toNum(dnum: Double) = Num(dnum)
  }
  
  object let {
    def apply(assignments: Asg*) = {
      new LetHelper(assignments.toList)
    }
  }
  
  class LetHelper (assignments: List[Asg]) {
    def in(nextExpr: Expr) = {
      def makeNestedLets(asgList: List[Asg]): Let = {
        asgList match {
          case Asg(Sym(name), value) :: Nil =>      Let(name, value, nextExpr)
          case Asg(Sym(name), value) :: moreAsgs => Let(name, value, makeNestedLets(moreAsgs))
          case _ => throw new Exception("Let expression: assignment required!")
        }
      }
      makeNestedLets(assignments)      
    }
  }
  
  
  //The concrete node types
  //Symbols: x
  case class Sym(name: String) extends Expr
  //Numbers: 23
  case class Num(number: Double) extends Expr
  //Addition: a + b
  case class Add(sum1: Expr, sum2: Expr) extends Expr
  //Power: a ** b
  case class Pow(base: Expr, expo: Expr) extends Expr
  //Assignment: x := a + b 
  case class Asg(lhs: Expr, rhs: Expr) extends Expr
  //Bind value to name, and evaluate next expression in new environment: 
  //let x := a + b in x ** 2
  case class Let(name: String, value: Expr, exprNext: Expr) extends Expr
}  

object TestDsl {
  def main(args : Array[String]) : Unit = {
    import testdsl._
//    import testdsl.Expr.let
    
    val x = new Sym("x")
    val a = new Sym("a")
    val e1 = let(x := a + 3 ) in x ** 2
    val e2 = let(x := 3) in (let(a := x + x) in a**2)
    val e3 = let(x := 3, a := x + x) in a**2
//    val e1 = let(x := a + 3 ) (x ** 2)
//    val e2 = let(x := 3)(let(a := x + x)(a**2))
//    val e3 = let(x := 3, a := x + x)(a**2)
//    
    println(e1)
    println(e2)
    println(e3)
    println("Finished")
  }
}
