/**
 * Demonstrate how the symbolic math libraries are used.
 * 
 * The three functions `useSyMathM`, `useSyMathV`, and `useSyMathOo` 
 * demonstrate the three different implementations of the
 * symbolic math library. As the libraries have the same functionality and 
 * interface, the functions should have (almost) the same code and produce 
 * the same output.  
 */
object UseTheLibraries {
  
  def useSyMathM() {
    println("Start of function `useSyMathM`. -----------------------")
    
    import symathm.Expression._
    import symathm.ExprOps._
    import Expr.{int2Num, double2Num}
    
    //Create some symbols (unknown variables)
    val (a, b, c, x) = (Sym("a"), Sym("b"), Sym("c"), Sym("x"))

    //Create an expression. `~^` denotes exponentiation (power).
    val expr1 = 2 * x~^4 + 5 * x~^2 + x~^0.5 
    //Print the expression.
    pprintln(expr1) 
    //Differentiate the expression with respect to `x`
    val dexpr1 = diff(expr1, x) 
    pprintln(dexpr1)
    //Evaluate the expression and its differential at x = 3.
    //The `Env` object creates an environment in which the expression is 
    //evaluated.
    val env1 = Env(x := 1)
    //val env1 = Environment("x" -> 1) //Possible as well
    pprintln(eval(expr1, env1))
    pprintln(eval(dexpr1, env1))
    
    //The library can also differentiate slightly more complicated powers
    pprintln(diff(a ~^ x, x))
    //but the result frequently looks fairly inelegant.
    pprintln(diff(x ~^ a, x))
    
    //There is a ML like `let` expression. It assigns values to variables.
    //This creates a new environment, where the assigned variables are known.
    //A single expression (after the keyword `in`) can be put into this new 
    //environment.
    //The `let` expression can be evaluated and differentiated.
    val expr2 = let(a := x ~^ 2, b := 2 * x) in a + b
    pprintln(eval(expr2)) //Empty environment is default
    pprintln(eval(expr2, Env(x := 1)))
    pprintln(diff(expr2, x))
    
    //As a byproduct of the let mechanism one can express known differentials
    //symbolically. The symbols that contain a "$" character in their names
    //are derivatives. "a$x" means: diff(a, x)
    val (a$x, b$x, c$x) = (Sym("a$x"), Sym("b$x"), Sym("c$x"))
    val env2 = Env(a$x := 0, b$x := 0, c$x := 0)
    pprintln(diff(a * b * c, x, env2))

    println()
    // `pprintln` has a debug mode that shows the structure of the expression 
    pprintln(2 * a ~^ 2, debug=true)
  }
  
  
  def useSyMathV() {
    println("\nStart of function `useSyMathV`. -----------------------")
  }
  
  
  def useSyMathOo() {
    println("\nStart of function `useSyMathOo`. -----------------------")
  }
  
  
  def main(args : Array[String]) : Unit = {
    useSyMathM()
    useSyMathV()
    useSyMathOo()
    
    println("\nProgram ended normally.")
  }
}
