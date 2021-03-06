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
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean =
      if (stagiumCoercePhase != null && global.phase.id == stagiumCoercePhase.id) {
        tpe1.isStaged == tpe2.isStaged || tpe1.isWildcard || tpe2.isWildcard
      } else {
        true
      }

    override def annotationsLub(tp: Type, ts: List[Type]): Type = {
      if (ts.exists(_.isStaged))
        tp.toStaged
      else
        tp.toDirect
    }

    override def annotationsGlb(tp: Type, ts: List[Type]): Type =
      annotationsLub(tp, ts)
  }
}
