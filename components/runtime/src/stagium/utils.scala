package stagium

object utils {

  import collection._

  // Schedule all instructions that need to be executed for x
  def schedule(x: Exp[_]): List[Exp[_]] = {
    val start = x
    val deps: mutable.Map[Sym[_], Set[Exp[_]]] = mutable.Map()

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