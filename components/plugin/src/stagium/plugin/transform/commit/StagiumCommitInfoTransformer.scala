package stagium.plugin
package transform
package commit

import scala.tools.nsc.transform.InfoTransform

trait StagiumCommitInfoTransformer extends InfoTransform {
  self: StagiumCommitPhase =>

  import global._
  import definitions._
  import helper._

  override def transformInfo(sym: Symbol, tpe: Type): Type =
    if (helper.flag_passive)
      tpe
    else
      deepTransformation(tpe)

  lazy val deepTransformation: TypeMap = new TypeMap {
    def apply(tpe: Type): Type = mapOver(tpe)
    override def mapOver(tpe: Type): Type = tpe match {
      case tpe if tpe != null && tpe.isStaged =>
        appliedType(ExpClass, tpe.toDirect)
      case _ =>
        super.mapOver(tpe)
    }
  }
}
