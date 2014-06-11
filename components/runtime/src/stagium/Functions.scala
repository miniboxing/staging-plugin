package stagium

import scala.reflect.runtime.universe._
import util._
import internal._

// this is the common ancestor of all functions in stagium
trait Fun[U]{
  def expr: Exp[U]
  def args: List[Arg[_]]
  def argsToString = args.map(a => a.n + ": " + a.tpeString).mkString("(", ", ", ")")
}

// and we have functions for 1 parameter ...
case class Fun1[T: TypeTag, U: TypeTag](f: Exp[T] => Exp[U]) extends Def[T => U] with Fun[U] {
  val a1 = Arg[T]("a1")
  val expr = f(a1)
  val args = List(a1)
}

// 2 parameters ...
case class Fun2[T1: TypeTag, T2: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2]) => Exp[U]) extends Def[(T1, T2) => U] with Fun[U] {
  val a1 = Arg[T1]("a1")
  val a2 = Arg[T2]("a2")
  val expr = f(a1, a2)
  val args = List(a1, a2)
}

// 4 parameters ...
case class Fun4[T1: TypeTag, T2: TypeTag, T3: TypeTag, T4: TypeTag, U: TypeTag](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[U]) extends Def[(T1, T2, T3, T4) => U] with Fun[U] {
  val a1 = Arg[T1]("a1")
  val a2 = Arg[T2]("a2")
  val a3 = Arg[T3]("a3")
  val a4 = Arg[T4]("a4")
  val expr = f(a1, a2, a3, a4)
  val args = List(a1, a2, a3, a4)
}

// and 8 parameters ...
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
