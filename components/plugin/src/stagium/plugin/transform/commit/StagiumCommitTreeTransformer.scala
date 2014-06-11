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
          afterCommit(checkNoStorage(unit, res))
        else
          EmptyTree
      }
  }

  def checkNoStorage(unit: CompilationUnit, tree: Tree) = {
    for (t <- tree)
      if(!isNotStaged(t.tpe))
        unit.error(t.pos, "Incorrect transformation for " + t + " that is typed using @staged past the commit phase: " + t.tpe)
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

  class StagiumTreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree0: Tree): Tree = {
      val oldTpe = tree0.tpe
      val newTpe = deepTransformation(oldTpe)

      // force new info on the symbol
      if (tree0.hasSymbolField)
        tree0.symbol.info

      val tree1 =
        tree0 match {

          // debug: I know I'm generating some redundant coercions, but I'm just too tired to debug it... :|
          case Direct2Staged(Staged2Direct(tree)) =>
            transform(tree)

          // going from staged to direct => make this a staging-time constant
          case Direct2Staged(arg) =>
            val con0 = gen.mkMethodCall(gen.mkAttributedIdent(ConClass.companionModule), List(transform(arg)))
            val con1 = localTyper.typed(con0)
            if (!(con1.tpe <:< newTpe))
              unit.error(tree0.pos, "Impossible to convert: " + con1.tpe + " to " + newTpe + " (internal tree: " + tree0 + ")")
            con1

          // going from staged to direct => no, no, that doesn't work
          case Staged2Direct(_) =>
            unit.error(tree0.pos, "Once in the staged world there's no going back, directly, use `execute` or `functionN` to go from  " + tree0.tpe + " to " + tree0.tpe.toStaged + " (internal debug: " + tree0 + ")")
            gen.mkAttributedRef(Predef_???)

          // execute => execute_impl
          case Apply(Apply(TypeApply(method, List(tpe)), List(exp)), List(tag)) if method.symbol == executeInterface =>
            val unstage = gen.mkAttributedIdent(executeImplement)
            val call = gen.mkMethodCall(gen.mkMethodCall(unstage, List(tpe.tpe), List(transform(exp))), List(tag))
            val tree2 = localTyper.typed(call)
            tree2

          // functionN => functionN_impl
          case Apply(Apply(TypeApply(method, tpes), List(exp)), tags) if functionInterfaces contains method.symbol =>
            val unstage = gen.mkAttributedIdent(functionImplements(method.symbol))
            val call = gen.mkMethodCall(gen.mkMethodCall(unstage, tpes.map(_.tpe), List(transform(exp))), tags)
            val tree2 = localTyper.typed(call)
            tree2

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }
}