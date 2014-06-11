import scala.reflect.runtime.universe._
import scala.language.implicitConversions
import scala.staged

import stagium.staging._
import stagium.internal._

package stagium {
  // an expression of any type
  sealed abstract class Exp[+T: TypeTag] {
    def tpeString: String = typeTag[T].tpe.toString
    override def toString = this match {
      case c: Con[_] => c.x.toString
      case s: Sym[_] => "x" + s.id
      case a: Arg[_] => a.n
    }
  }
  // ... that can be a constant:
  case class Con[T: TypeTag](x: T) extends Exp[T]
  // ... a function argument:
  case class Arg[T: TypeTag](n: String) extends Exp[T]
  // ... or a symbol:
  case class Sym[T: TypeTag](id: Int) extends Exp[T]
  // ... which, in turn represents a definition:
  abstract class Def[T]
  // ... which you can customize!
}

package object stagium {

  // the interfaces for the gateways from the staged world back to the direct world:
  def execute[T: TypeTag](t: T @staged): T = t
  def function1[T: TypeTag, U: TypeTag](f: T @staged => U @staged): T => U = f
  def function2[T1: TypeTag, T2: TypeTag, U: TypeTag](f: (T1 @staged, T2 @staged) => U @staged): (T1, T2) => U = f
  def function4[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, U: TypeTag](f: (T1 @staged, T2 @staged, T3 @staged, T4 @staged) => U @staged): (T1, T2, T3, T4) => U = f
  def function8[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, T5: TypeTag, T6: TypeTag, T7: TypeTag, T8: TypeTag, U: TypeTag](f: (T1 @staged, T2 @staged, T3 @staged, T4 @staged, T5 @staged, T6 @staged, T7 @staged, T8 @staged) => U @staged): (T1, T2, T3, T4, T5, T6, T7, T8) => U = f

  // artifact necessary to have definitions automatically assigned symbols
  implicit def addDef[T: TypeTag](d: Def[T]): Sym[T] = internal.addDefInternal[T](d)
}

