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

  object Staged2Direct {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(_, arg :: Nil) if tree.symbol == staged2direct => Some(arg)
      case _ => None
    }
  }

  object Direct2Staged {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(_, arg :: Nil) if tree.symbol == direct2staged => Some(arg)
      case _ => None
    }
  }

  object AsInstanceOf {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Apply(TypeApplyOp(tree, Any_asInstanceOf, tpe :: Nil), Nil) => Some((tree, TypeTree(tpe)))
      case _ => None
    }
  }

  object MaybeTypeApply {
    def unapply(tree: Tree): Option[(Tree, List[Type])] = tree match {
      case TypeApply(fun, targs)           => Some((fun, targs.map(_.tpe)))
      case _ if tree.tpe.typeParams == Nil => Some((tree, Nil))
      case _                               => None
    }

    def apply(tree: Tree, targs: List[Type]): Tree =
      if (targs.isEmpty)
        tree
        else
          TypeApply(tree, targs.map(TypeTree(_)))
  }

  object MaybeApply {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case Apply(fun, args)             => Some((fun, args))
      case _ if tree.tpe.paramss == Nil => Some((tree, Nil))
      case _                            => None
    }

    def apply(tree: Tree, args: List[Tree]): Tree =
      if (args.isEmpty)
        tree
      else
        Apply(tree, args)
  }

  def direct2staged(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(direct2staged), List(tree)) setType tree.tpe.toStaged)
  def staged2direct(tree: Tree): Tree = atPos(tree.pos)(Apply(gen.mkAttributedRef(staged2direct), List(tree)) setType tree.tpe.toDirect)
//  def direct2staged(sym: Symbol): Tree = direct2staged(gen.mkAttributedRef(sym))
//  def staged2direct(tree: Tree, x: Symbol): Tree = atPos(tree.pos)(Selectx(staged2direct(tree), x))
//  def staged2direct(sym: Symbol): Tree = staged2direct(gen.mkAttributedRef(sym))
//  def staged2direct(sym: Symbol, x: Symbol): Tree = staged2direct(gen.mkAttributedRef(sym), x)
}