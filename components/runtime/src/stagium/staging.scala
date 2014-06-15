package stagium

import scala.reflect.runtime.universe._
import internal._

object staging {

  import collection._

  // this method stages an expression by ...
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

  // ... calling the code generator for the expression ....
  def codeFor[T](x: Exp[T], indent: Int): String = {
    val sb = new StringBuffer()
    def append(s: String, nl: Boolean = true) = sb.append("  " * indent + s + (if (nl) "\n" else ""))
    append("{")
    var end = ""
    x match {
      case Def(f: Fun[_]) =>
        append(s"  val $x: ${x.tpeString} =")
        append(s"    ${f.argsToString} =>", nl = false)
        append(codeFor(f.expr, indent + 2))
        end = "end of function " + x + ": " + x.tpeString
      case _ =>
        val sched = schedule(x)
        for (sym <- sched) {
          sym match {
            case s: Sym[_] =>
              append(s"  val $s: ${s.tpeString} = ${defs(s)}")
            case other =>
              append(other.toString)
          }
        }
        end = "end of code block of " + sched.length + " instructions"
    }
    append("  " + x + ": " + x.tpeString)
    append("} // " + end)
    sb.toString
  }

  // ... which, in turn, calls the scheduler:
  def schedule(x: Exp[_]): List[Exp[_]] = {
    val start = x
    val deps: mutable.Map[Sym[_], Set[Exp[_]]] = mutable.Map()
    if (!x.isInstanceOf[Sym[_]])
      return List()

    def transitive_closure(start: Set[Exp[_]]): Set[Exp[_]] = {
      val trans = start ++ start.flatMap { x => x match {
        case s: Sym[_] =>
          s match {
            case Def(f: Fun[_]) =>
              deps += ((s,  Set(f.expr)))
              List[Exp[_]](f.expr)
            case s @ Def(p: Product) =>
              val l = p.productIterator.toList.collect({ case s: Sym[_] => s })
              deps += ((s,  l.toSet))
              l
            case s: Sym[_] =>
              List[Sym[_]](s)
          }
        case _ => ???
      }}
      val transs = trans.toSet
      if (transs.size == start.size)
        transs
      else
        transitive_closure(transs)
    }


    var code: List[Exp[_]] = Nil
    var work: Set[Exp[_]] = transitive_closure(Set(start))

    while (!work.isEmpty) {
      val out = work.collect {
        case s: Sym[_] if (work intersect deps(s)).isEmpty =>
          s // symbol with no dependencies
        case c: Con[_] =>
          c // constant
      }
      code = code ::: out.toList
      work = work -- out.asInstanceOf[Set[Exp[_]]]
    }

    code
  }
}
