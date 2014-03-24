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

  def unstage[T](t: Exp[T])(implicit ttag: TypeTag[T], stager: Stager): T =
    stager.stage(t)

  // TODO: Redirect this to the actual real unstage
  def unstage[T](t: T @staged)(implicit ttag: TypeTag[T], stager: Stager): T =
    t
}

package stagium {

  class Exp[+T: TypeTag]
  case class Con[T: TypeTag](x: T) extends Exp[T]
  case class Arg[T: TypeTag](n: String) extends Exp[T] // TODO
  case class Sym[T: TypeTag](id: Int) extends Exp[T]
  abstract class Def[T]

  trait Stager {
    def stage[T: TypeTag](x: Exp[T]): T
  }
}