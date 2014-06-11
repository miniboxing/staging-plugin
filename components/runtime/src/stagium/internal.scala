package stagium

import scala.reflect.runtime.universe._
import staging._

object internal {

  // the implementations of the gateways from the staged world back to the direct world:
  def execute_impl[T: TypeTag](t: Exp[T]): T = stage(t)
  def function1_impl[T: TypeTag, U: TypeTag](f: Exp[T] => Exp[U]): T => U = { stage(addDef(Fun1(f))) }
  def function2_impl[T1: TypeTag, T2: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2]) => Exp[U]): (T1, T2) => U = { stage(addDef(Fun2(f))) }
  def function4_impl[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[U]): (T1, T2, T3, T4) => U = { stage(addDef(Fun4(f))) }
  def function8_impl[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, T5: TypeTag, T6: TypeTag, T7: TypeTag, T8: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8]) => Exp[U]): (T1, T2, T3, T4, T5, T6, T7, T8) => U = { stage(addDef(Fun8(f))) }

  // symbol ids
  var sidx = 0

  // definitions
  var defs = Map[Sym[_], Def[_]]()

  // useful extractor
  object Def {
    def unapply(e: Exp[_]): Option[Def[_]] = {
      defs.filter(_._1 == e).toList match {
        case List((_, d)) => Some(d)
        case _ => None
      }
    }
  }

  def addDefInternal[T: TypeTag](d: Def[T]): Sym[T] = {
    if (cse && defs.valuesIterator.contains(d)) {
      defs.find(_._2 == d).get._1.asInstanceOf[Sym[T]]
    } else {
      val sym = Sym[T](sidx)
      defs += sym -> d
      sidx += 1
      sym
    }
  }

  private[this] var cse = false
  def enableCommonSubexpressionElimination(): Unit = { cse = true }
}