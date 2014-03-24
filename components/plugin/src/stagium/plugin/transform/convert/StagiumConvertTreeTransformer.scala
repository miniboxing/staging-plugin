package stagium.plugin
package transform
package convert

import scala.tools.nsc.typechecker.Analyzer
import scala.reflect.internal.Mode

trait StagiumConvertTreeTransformer {
  this: StagiumConvertPhase =>

  import global._
  import definitions._
  import treeInfo.{AsInstanceOf => _, _}
  import helper._
  import Flag._

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    val stageTrans = new StagiumTreeTransformer(unit)
    override def transform(tree: Tree): Tree =
      afterConvert(checkNoStorage(stageTrans.transform(tree)))
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
//      println(oldTpe + " ==> " + newTpe)

      // force new info on the symbol
      if (tree0.hasSymbolField)
        tree0.symbol.info

      val tree1 =
        tree0 match {

          case Select(Staged2Direct(tree, targ), method0) =>
            println(tree0)
            val tree1 = transform(tree)
            val staged = Ident(TermName("__staged"))
            val method = TermName("infix_" + method0)
            val infixm = Select(staged, method)
            val infixa = Apply(infixm, List(tree1))
            println(infixa)
            localTyper.typedOperator(infixa)
            //transform(tree)

          case _ =>
            super.transform(tree0)
        }

      tree1.setType(newTpe)
    }
  }

//    class ConvertPhase(prev: Phase) extends StdPhase(prev) {
//    override def name = StagiumConvertTreeTransformer.this.phaseName
//    override def checkable = false
//    def apply(unit: CompilationUnit): Unit = {
//      val tree = afterConvert(new TreeAdapters().adapt(unit))
//      tree.foreach(node => if (!node.isInstanceOf[Import] && node.tpe == null) unit.error(node.pos, s"[stagium-coerce] tree not typed: $tree"))
//      def isFlapping(tree: Tree) = tree match {
//        case Unbox2box(Box2unbox(_)) => true
//        case Box2unbox(Unbox2box(_)) => true
//        case _ => false
//      }
//      tree.collect{ case sub if isFlapping(sub) => unit.error(sub.pos, s"unexpected leftovers after coerce: $sub") }
//    }
//  }
//
//  class TreeAdapters extends Analyzer {
//    var indent = 0
//    def adaptdbg(ind: Int, msg: => String): Unit = stagiumlog("  " * ind + msg)
//
//    lazy val global: StagiumConvertTreeTransformer.this.global.type = StagiumConvertTreeTransformer.this.global
//    override def newTyper(context: Context): Typer = new TreeAdapter(context)
//
//    def adapt(unit: CompilationUnit): Tree = {
//      val context = rootContext(unit)
//      val checker = new TreeAdapter(context)
//      unit.body = checker.typed(unit.body)
//      unit.body
//    }
//
//    class TreeAdapter(context0: Context) extends Typer(context0) {
//      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
//        templ
//
//      def supertyped(tree: Tree, mode: Mode, pt: Type): Tree =
//        super.typed(tree, mode, pt)
//
//      override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
//        val oldTpe = tree.tpe
//        val newTpe = pt
//        def typeMismatch = oldTpe.isStaged ^ newTpe.isStaged
//        def dontAdapt = tree.isType || pt.isWildcard
//        if (typeMismatch && !dontAdapt) {
//          val conversion = if (oldTpe.isStaged) staged2direct else direct2staged
//          val convertee = if (oldTpe.typeSymbol.isBottomClass) gen.mkAttributedCast(tree, newTpe.toDirect) else tree
//          val tree1 = atPos(tree.pos)(Apply(gen.mkAttributedRef(conversion), List(convertee)))
//          val tree2 = super.typed(tree1, mode, pt)
//          assert(tree2.tpe != ErrorType, tree2)
//          tree2
//        } else {
//          super.adapt(tree, mode, pt, original)
//        }
//      }
//
//      override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
//        val ind = indent
//        indent += 1
//        adaptdbg(ind, " <== " + tree + ": " + showRaw(pt, true, true, false, false))
//
//        def fallback() = super.typed(tree, mode, pt)
//        def retypecheck() = super.typed(tree.clearType(), mode, pt)
//
//        val res = tree match {
//          case EmptyTree | TypeTree() =>
//            fallback()
//
//          case _ if tree.tpe == null =>
//            fallback()
//
//          case Select(Staged2Direct(tree, targ), method) =>
//            println(tree)
//            val staged = Ident(TermName("__staged"))
//            val infix  = Select(staged, TermName("infix_" + method))
//            super.typed(tree)
//
//          case _ =>
//            retypecheck()
//        }
//
//        adaptdbg(ind, " ==> " + res + ": " + res.tpe)
//        if (res.tpe == ErrorType) adaptdbg(ind, "ERRORS: " + context.errors)
//        indent -= 1
//        res
//      }
//    }
//  }
}