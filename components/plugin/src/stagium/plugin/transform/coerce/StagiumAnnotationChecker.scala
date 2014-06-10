package stagium.plugin
package transform
package coerce

trait StagiumAnnotationCheckers {
  this: StagiumCoercePhase =>

  import global._
  import definitions._
  import helper._

  object ValueAnnotationChecker extends AnnotationChecker{

    /**
     *  LDL FTW -- Boil frog, boil!
     */
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      // we run localTyper during convert during code synthesis, and there we need `T @unboxed` and `Any` to be compatible again
      if (stagiumCoercePhase != null && global.phase.id == stagiumCoercePhase.id) {
        tpe1.isStaged == tpe2.isStaged || tpe1.isWildcard || tpe2.isWildcard
      } else {
        true
      }
    }

    override def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      println()
      println("adaptBounds: " + bounds)
      println(tparams)
      println(targs)
      bounds
    }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      println()
      println("lub: " + tp + " from " + ts)
      ???
      if (ts.exists(_.isStaged))
        tp.toStaged
      else
        tp.toDirect
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type =
      annotationsLub(tp, ts)
  }
}
