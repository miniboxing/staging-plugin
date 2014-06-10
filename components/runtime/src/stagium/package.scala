import scala.reflect.runtime.universe._
import scala.staged

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

  def execute_impl[T](t: Exp[T])(implicit ttag: TypeTag[T], stager: Stager): T =
    stager.stage(t)

  // TODO: Redirect this to the actual real unstage
  def execute[T](t: T @staged)(implicit ttag: TypeTag[T], stager: Stager): T =
    t

  implicit object DefaultStager extends Stager {
    def stage[T: TypeTag](x: Exp[T]): T = {
      println("Need to compile and run:")
      println("*********************************")
      println("{")
      for (sym <- utils.schedule(x)) {
        sym match {
          case s: Sym[_] =>
            println("  val " + s + ": " + s.tpeString + " = " + defs(s))
          case other =>
            println(other)
        }
      }
      println("  " + x + ": " + x.tpeString)
      println("}")
      println("*********************************")

      null.asInstanceOf[T]
    }
  }
}

package stagium {

  sealed abstract class Exp[+T: TypeTag] {
    def tpeString: String = typeTag[T].tpe.toString
    override def toString = this match {
      case c: Con[_] => c.x.toString
      case s: Sym[_] => "x" + s.id
      case a: Arg[_] => ???
    }
  }
  case class Con[T: TypeTag](x: T) extends Exp[T]      // Ugly hack until we get type tags
  case class Arg[T: TypeTag](n: String) extends Exp[T] // TODO
  case class Sym[T: TypeTag](id: Int) extends Exp[T]
  abstract class Def[T]

  trait Stager {
    def stage[T: TypeTag](x: Exp[T]): T
  }
}