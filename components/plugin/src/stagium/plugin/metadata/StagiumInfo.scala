package stagium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.language.implicitConversions
import scala.reflect.internal.Flags._

trait StagiumInfo {
  this: StagiumHelper =>

  import global._
  import treeInfo._
  import definitions._

  implicit class RichTree(tree: Tree) {
    def isStaged = tree.tpe.isStaged
  }

  implicit class RichSymbol(sym: Symbol) {
    def isStaged = sym.info.isStaged
  }

  implicit class RichType(tpe: Type) {
    def isStaged = tpe.dealiasWiden.hasAnnotation(StagedClass)
    def toStaged = if (tpe.isStaged) tpe else tpe.withAnnotation(AnnotationInfo(StagedClass.tpe, Nil, Nil))
    def toDirect = if (tpe.isStaged) tpe.filterAnnotations(_.tpe.typeSymbol != StagedClass) else tpe
  }

  object Unbox2box {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(_, arg :: Nil) if tree.symbol == unbox2box => Some(arg)
      case _ => None
    }
  }

  object Box2unbox {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(_, arg :: Nil) if tree.symbol == box2unbox => Some(arg)
      case _ => None
    }
  }

  object AsInstanceOf {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Apply(TypeApplyOp(tree, Any_asInstanceOf, tpe :: Nil), Nil) => Some((tree, TypeTree(tpe)))
      case _ => None
    }
  }

//  def box2unbox(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(box2unbox), List(tree)) setType tree.tpe.toStaged)
//  def box2unbox(sym: Symbol): Tree = box2unbox(gen.mkAttributedRef(sym))
//  def unbox2box(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(unbox2box), List(tree)) setType tree.tpe.toDirect)
//  def unbox2box(tree: Tree, x: Symbol): Tree = atPos(tree.pos)(Selectx(unbox2box(tree), x))
//  def unbox2box(sym: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym))
//  def unbox2box(sym: Symbol, x: Symbol): Tree = unbox2box(gen.mkAttributedRef(sym), x)
}