
object Test {
  def main(args: Array[String]): Unit = {

    def pow1(exp: Double, pow: Int): Double =
      if ( pow == 0 ) 1.0
      else if ( pow % 2 == 1 ) exp * pow1(exp, pow - 1)
      else { // pow % 2 == 0
        val x = pow1(exp, pow/2)
        x * x
      }
    def compile1(f: Double => Double) = f
    def pow1_5(exp: Double) = compile1(pow1(_, 5))(exp)
    println(s"pow1_5(3.0) = ${pow1_5(3.0)}")

    // staging engine
    sealed abstract class Staged {
      override def toString: String = {
        "<staged>"
      }
    }
    case class Const(x: Any) extends Staged
    case class Arg(n: String) extends Staged // TODO
    case class Sym(id: Int) extends Staged
    abstract class Def
    var sidx = 0
    var defs = Map[Sym, Def]()
    def addDef(d: Def): Sym = {
      val sym = Sym(sidx)
      defs += sym -> d
      sidx += 1
      sym
    }

    // integer staging
    case class Mult(o1: Staged, o2: Staged) extends Def
    def infix_*(o1: Staged, o2: Staged): Staged = addDef(Mult(o1, o2))

    def pow2(exp: Staged, pow: Int): Staged =
      if ( pow == 0 ) Const(1.0)
      else if ( pow % 2 == 1 ) infix_*(exp, pow2(exp, pow - 1))
      else { // pow % 2 == 0
        val x = pow2(exp, pow/2)
        infix_*(x, x)
      }
    def compile2(f: Staged => Staged) = f // TODO
    def pow2_5(exp: Staged) = compile2(pow2(_, 5))(exp)
    println(s"pow2_5(3.0) = ${pow2_5(Const(3.0))}")
  }
}
