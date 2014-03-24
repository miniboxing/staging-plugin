package stagium.plugin
package transform
package coerce

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Phase
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.util.DynamicVariable

trait StagiumCoerceTreeTransformer {
  this: StagiumCoercePhase =>

  import global._
  import definitions._
  import helper._

  class CoercePhase(prev: Phase) extends StdPhase(prev) {
    override def name = StagiumCoerceTreeTransformer.this.phaseName
    override def checkable = false
    def apply(unit: CompilationUnit): Unit = {
      val tree = afterCoerce(new TreeAdapters().adapt(unit))
      tree.foreach(node => if (!node.isInstanceOf[Import] && node.tpe == null) unit.error(node.pos, s"[stagium-coerce] tree not typed: $tree"))
      def isFlapping(tree: Tree) = tree match {
        case Unbox2box(Box2unbox(_)) => true
        case Box2unbox(Unbox2box(_)) => true
        case _ => false
      }
      tree.collect{ case sub if isFlapping(sub) => unit.error(sub.pos, s"unexpected leftovers after coerce: $sub") }
    }
  }

  class TreeAdapters extends Analyzer {
    var indent = 0
    def adaptdbg(ind: Int, msg: => String): Unit = stagiumlog("  " * ind + msg)

    lazy val global: StagiumCoerceTreeTransformer.this.global.type = StagiumCoerceTreeTransformer.this.global
    override def newTyper(context: Context): Typer = new TreeAdapter(context)

    def adapt(unit: CompilationUnit): Tree = {
      val context = rootContext(unit)
      val checker = new TreeAdapter(context)
      unit.body = checker.typed(unit.body)
      unit.body
    }

    class TreeAdapter(context0: Context) extends Typer(context0) {
      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
        templ

      def supertyped(tree: Tree, mode: Mode, pt: Type): Tree =
        super.typed(tree, mode, pt)

      override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
        val oldTpe = tree.tpe
        val newTpe = pt
        def typeMismatch = oldTpe.isStaged ^ newTpe.isStaged
        def dontAdapt = tree.isType || pt.isWildcard
        if (typeMismatch && !dontAdapt) {
          val conversion = if (oldTpe.isStaged) staged2direct else direct2staged
          val convertee = if (oldTpe.typeSymbol.isBottomClass) gen.mkAttributedCast(tree, newTpe.toDirect) else tree
          val tree1 = atPos(tree.pos)(Apply(gen.mkAttributedRef(conversion), List(convertee)))
          val tree2 = super.typed(tree1, mode, pt)
          assert(tree2.tpe != ErrorType, tree2)
          tree2
        } else {
          super.adapt(tree, mode, pt, original)
        }
      }

      override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
        val ind = indent
        indent += 1
        adaptdbg(ind, " <== " + tree + ": " + showRaw(pt, true, true, false, false))

        def fallback() = super.typed(tree, mode, pt)
        def retypecheck() = super.typed(tree.clearType(), mode, pt)

        val res = tree match {
          case EmptyTree | TypeTree() =>
            fallback()

          case _ if tree.tpe == null =>
            fallback()

          case Select(qual, meth) if qual.isTerm && tree.symbol.isMethod =>
            val qual2 = super.typed(qual.clearType(), mode | QUALmode, WildcardType)
            if (qual2.isStaged) {
              val tpe2 = if (qual2.tpe.hasAnnotation(StagedClass)) qual2.tpe else qual2.tpe.widen
              val tpe3 = tpe2.toDirect
              val qual3 = super.typed(qual.clearType(), mode, tpe3)
              super.typed(Select(qual3, meth) setSymbol tree.symbol, mode, pt)
            } else {
              retypecheck()
            }

          case _ =>
            retypecheck()
        }

        adaptdbg(ind, " ==> " + res + ": " + res.tpe)
        if (res.tpe == ErrorType) adaptdbg(ind, "ERRORS: " + context.errors)
        indent -= 1
        res
      }
    }
  }
}