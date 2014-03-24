package stagium.plugin
package transform
package addext

import scala.tools.nsc.transform.InfoTransform

trait StagiumAddExtInfoTransformer extends InfoTransform {
  self: StagiumAddExtensionMethodsPhase =>

  import global._
  import definitions._

  override def transformInfo(sym: Symbol, tpe: Type): Type = tpe
}
