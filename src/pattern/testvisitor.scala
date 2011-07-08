package pattern

/** Implement the visitor pattern.
  *
  */
package testvisitor {
  abstract class Expr {
    //import AstOps.{ flatten_add, flatten_mul }
  
    def +(other: Expr) = Add(this :: other :: Nil)
    def -(other: Expr) = Add(this :: Neg(other) :: Nil)
    
    def accept(v:Visitor): String
  }
  
  //The concrete node types
  case class Num(num: Double) extends Expr {
    def accept(v: Visitor) = v.visitNum(this)
  }
  case class Sym(name: String) extends Expr {
    def accept(v: Visitor) = v.visitSym(this)
  }
  case class Neg(term: Expr) extends Expr {
    def accept(v: Visitor) = v.visitNeg(this)
  }
  case class Add(summands: List[Expr]) extends Expr {
    def accept(v: Visitor) = v.visitAdd(this)
  }
  
  
  abstract class Visitor {
    def visitNum(num: Num): String
    def visitSym(sym: Sym): String 
    def visitNeg(neg: Neg): String 
    def visitAdd(add: Add): String 
  }
  
  class PrettyStrVisitor extends Visitor{
    //Convert elements of `terms` to strings,
    //and place string `sep` between them
    def convert_join(sep: String, terms: List[Expr]) = {
      val str_lst = terms.map { _.accept(this) }
      str_lst.reduce((s1, s2) => s1 + sep + s2)
    }

    def visitNum(num: Num) = num.num.toString()
    def visitSym(sym: Sym) = sym.name
    def visitNeg(neg: Neg) = "-" + neg.term.accept(this)
    def visitAdd(add: Add) = convert_join(" + ", add.summands)
  }
}
object TestVisitor {
  def main(args: Array[String]): Unit = {
    import testvisitor._
    
    val v = new PrettyStrVisitor()
    
    val x = Sym("x")
    val two = Num(2)

    println(x.accept(v))
    println(two.accept(v))
    println(Neg(x).accept(v))
    println(Add(x :: two :: Nil).accept(v))
    
    println("Finshed")
  }
}
