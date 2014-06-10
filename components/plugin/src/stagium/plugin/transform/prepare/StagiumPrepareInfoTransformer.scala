package stagium.plugin
package transform
package prepare

import scala.tools.nsc.transform.InfoTransform

trait StagiumPrepareInfoTransformer extends InfoTransform {
  self: StagiumPreparePhase =>

  import global._
  import definitions._
  import helper._

  // this bridges the gap between staged and unstaged methods
  // At this point, everything is staged
  override def transformInfo(sym: Symbol, tpe: Type): Type =
    if (helper.flag_passive)
      tpe
    else
      deepTransformation(tpe)

  lazy val deepTransformation: TypeMap = new TypeMap {
    def apply(tpe: Type): Type = mapOver(tpe)
    override def mapOver(tpe: Type): Type = tpe match {
      case TypeRef(_, ExpClass, List(tpe)) =>
        tpe.toStaged
      case _ =>
        super.mapOver(tpe)
    }
  }
}
