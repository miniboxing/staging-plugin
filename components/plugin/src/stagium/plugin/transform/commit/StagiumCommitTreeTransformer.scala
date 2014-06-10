package stagium.plugin
package transform
package commit

import scala.tools.nsc.typechecker.Analyzer
import scala.reflect.internal.Mode

trait StagiumCommitTreeTransformer {
  this: StagiumCommitPhase =>

  import global._
  import definitions._
  import treeInfo.{AsInstanceOf => _, _}
  import helper._
  import Flag._

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    val stageTrans = new StagiumTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      if (helper.flag_passive)
        tree
      else {
        val res = afterCommit(stageTrans.transform(tree))
        if (!reporter.hasErrors)
          afterCommit(checkNoStorage(res))
        else
          EmptyTree
      }
  }

  def checkNoStorage(tree: Tree) = {
    for (t <- tree)
      assert(isNotStaged(t.tpe), t + ": " + t.tpe)
    tree
  }

  def isNotStaged(t: Type): Boolean = {
    var isStaged = false
    new TypeMap {
      def apply(tp: Type): Type = mapOver(tp)
      override def mapOver(tp: Type): Type = tp match {
        case _ if tp != null && tp.isStaged =>
          isStaged = true
          tp
        case _ =>
          super.mapOver(tp)
      }
    }.apply(t)

    !isStaged
  }

  abstract sealed class Constraint
  case object Miniboxed extends Constraint
  case object Boxed extends Constraint
  case object NoConstraint extends Constraint

  class CoercionExtractor {
    def unapply(tree: Tree, sym: Symbol): Option[(Tree, Type)] = tree match {
      case Apply(TypeApply(fun, List(targ)), List(inner)) if fun.symbol == sym => Some((inner, targ.tpe))
      case _ => None
    }
  }

  object Staged2Direct extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type)] = unapply(tree, staged2direct)
  }

  object Direct2Staged extends CoercionExtractor {
    def unapply(tree: Tree): Option[(Tree, Type)] = unapply(tree, direct2staged)
  }

  class StagiumTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree0: Tree): Tree = {
      val oldTpe = tree0.tpe
      val newTpe = deepTransformation(oldTpe)

      // force new info on the symbol
      if (tree0.hasSymbolField)
        tree0.symbol.info

      val tree1 =
        tree0 match {
          case Direct2Staged(arg, tpe) =>
            val con0 = gen.mkMethodCall(gen.mkAttributedIdent(ConClass.companionModule), List(transform(arg)))
            val con1 = localTyper.typed(con0)
            if (!(con1.tpe <:< newTpe))
              unit.error(tree0.pos, "Mismatching types: " + con1.tpe + " <:< " + newTpe)
            con1

          case Staged2Direct(_, _) =>
            unit.error(tree0.pos, "Once in the staged world there's no going back, directly, use `execute` or `function` to go from  " + tree0.tpe + " to " + tree0.tpe.toDirect + " (internal debug: " + tree0 + ")")
            gen.mkAttributedRef(Predef_???)

          case Apply(Apply(TypeApply(method, List(tpe)), List(exp)), List(tag, stager)) if method.symbol == unstageInterface =>
            val unstage = gen.mkAttributedIdent(unstageImplment)
            val call = gen.mkMethodCall(gen.mkMethodCall(unstage, List(tpe.tpe), List(transform(exp))), List(tag, stager))
            println(call)
            val tree2 = localTyper.typed(call)
            tree2

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}