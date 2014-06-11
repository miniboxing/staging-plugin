import scala.reflect.runtime.universe._
import scala.staged
import scala.annotation.unchecked.uncheckedVariance

package object stagium {

  private[this] var sidx = 0
  private[this] var defs = Map[Sym[_], Def[_]]()

  def addDef[T: TypeTag](d: Def[T]): Sym[T] = {
    val sym = Sym[T](sidx)
    defs += sym -> d
    sidx += 1
    sym
  }

  object Def {
    def unapply(e: Exp[_]): Option[Def[_]] = {
      defs.filter(_._1 == e).toList match {
        case List((_, d)) => Some(d)
        case _ => None
      }
    }
  }

  def execute[T: TypeTag](t: T @staged): T = t
  def execute_impl[T: TypeTag](t: Exp[T]): T = stage(t)

  def function1[T: TypeTag, U: TypeTag](f: T @staged => U @staged): T => U = f
  def function2[T1: TypeTag, T2: TypeTag, U: TypeTag](f: (T1 @staged, T2 @staged) => U @staged): (T1, T2) => U = f
  def function4[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, U: TypeTag](f: (T1 @staged, T2 @staged, T3 @staged, T4 @staged) => U @staged): (T1, T2, T3, T4) => U = f
  def function8[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, T5: TypeTag, T6: TypeTag, T7: TypeTag, T8: TypeTag, U: TypeTag](f: (T1 @staged, T2 @staged, T3 @staged, T4 @staged, T5 @staged, T6 @staged, T7 @staged, T8 @staged) => U @staged): (T1, T2, T3, T4, T5, T6, T7, T8) => U = f
  def function1_impl[T: TypeTag, U: TypeTag](f: Exp[T] => Exp[U]): T => U = { stage(addDef(Fun1(f))) }
  def function2_impl[T1: TypeTag, T2: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2]) => Exp[U]): (T1, T2) => U = { stage(addDef(Fun2(f))) }
  def function4_impl[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[U]): (T1, T2, T3, T4) => U = { stage(addDef(Fun4(f))) }
  def function8_impl[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, T5: TypeTag, T6: TypeTag, T7: TypeTag, T8: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8]) => Exp[U]): (T1, T2, T3, T4, T5, T6, T7, T8) => U = { stage(addDef(Fun8(f))) }


  def codeFor[T](x: Exp[T], indent: Int): String = {
    val sb = new StringBuffer()
    def append(s: String, nl: Boolean = true) = sb.append("  " * indent + s + (if (nl) "\n" else ""))
    append("{")
    x match {
      case Def(f: Fun[_]) =>
        append(s"  val $x: ${x.tpeString} =")
        append(s"    ${f.argsToString} =>", nl = false)
        append(codeFor(f.expr, indent + 2))
      case _ =>
        for (sym <- utils.schedule(x)) {
          sym match {
            case s: Sym[_] =>
              append(s"  val $s: ${s.tpeString} = ${defs(s)}")
            case other =>
              append(other.toString)
          }
        }
    }
    append("  " + x + ": " + x.tpeString)
    append("}", nl = false)
    sb.toString
  }

  def stage[T: TypeTag](x: Exp[T]): T = {
    println("Need to compile and run:")
    println("*********************************")
    println(codeFor(x, 0))
    println("*********************************")
    // unfortunately at this point we don't have the code
    // to run and compile the program, therefore we need to
    // return a value:
    (x match {
      case Con(x)                          => x
      case Def(_: Fun1[_,_])               => ((_: AnyRef) => { println("<function1 called>"); null })
      case Def(_: Fun2[_,_,_])             => ((_: AnyRef,_: AnyRef) => { println("<function2 called>"); null })
      case Def(_: Fun4[_,_,_,_,_])         => ((_: AnyRef,_: AnyRef,_: AnyRef,_: AnyRef) => { println("<function4 called>"); null })
      case Def(_: Fun8[_,_,_,_,_,_,_,_,_]) => ((_: AnyRef,_: AnyRef,_: AnyRef,_: AnyRef,_: AnyRef,_: AnyRef,_: AnyRef,_: AnyRef) => { println("<function8 called>"); null })
      case Sym(_)                          => null
    }).asInstanceOf[T]
  }
}

package stagium {

  sealed abstract class Exp[+T: TypeTag] {
    def tpeString: String = typeTag[T].tpe.toString
    override def toString = this match {
      case c: Con[_] => c.x.toString
      case s: Sym[_] => "x" + s.id
      case a: Arg[_] => a.n
    }
  }
  case class Con[T: TypeTag](x: T) extends Exp[T]
  case class Arg[T: TypeTag](n: String) extends Exp[T]
  case class Sym[T: TypeTag](id: Int) extends Exp[T]

  abstract class Def[T]

  trait Fun[U]{
    def expr: Exp[U]
    def args: List[Arg[_]]
    def argsToString = args.map(a => a.n + ": " + a.tpeString).mkString("(", ", ", ")")
  }
  case class Fun1[T: TypeTag, U: TypeTag](f: Exp[T] => Exp[U]) extends Def[T => U] with Fun[U] {
    val a1 = Arg[T]("a1")
    val expr = f(a1)
    val args = List(a1)
  }
  case class Fun2[T1: TypeTag, T2: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2]) => Exp[U]) extends Def[(T1, T2) => U] with Fun[U] {
    val a1 = Arg[T1]("a1")
    val a2 = Arg[T2]("a2")
    val expr = f(a1, a2)
    val args = List(a1, a2)
  }
  case class Fun4[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[U]) extends Def[(T1, T2, T3, T4) => U] with Fun[U] {
    val a1 = Arg[T1]("a1")
    val a2 = Arg[T2]("a2")
    val a3 = Arg[T3]("a3")
    val a4 = Arg[T4]("a4")
    val expr = f(a1, a2, a3, a4)
    val args = List(a1, a2, a3, a4)
  }
  case class Fun8[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, T5: TypeTag, T6: TypeTag, T7: TypeTag, T8: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8]) => Exp[U]) extends Def[(T1, T2, T3, T4, T5, T6, T7, T8) => U] with Fun[U] {
    val a1 = Arg[T1]("a1")
    val a2 = Arg[T2]("a2")
    val a3 = Arg[T3]("a3")
    val a4 = Arg[T4]("a4")
    val a5 = Arg[T5]("a5")
    val a6 = Arg[T6]("a6")
    val a7 = Arg[T7]("a7")
    val a8 = Arg[T8]("a8")
    val expr = f(a1, a2, a3, a4, a5, a6, a7, a8)
    val args = List(a1, a2, a3, a4, a5, a6, a7, a8)
  }
}