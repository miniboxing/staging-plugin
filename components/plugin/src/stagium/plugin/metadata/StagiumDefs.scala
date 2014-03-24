package stagium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait StagiumDefs {
  this: StagiumHelper =>

  import global._
  import definitions._

  lazy val StagedClass = rootMirror.getRequiredClass("scala.staged")

  lazy val ExpClass = rootMirror.getRequiredClass("stagium.Exp")

  // artificially created marker methods
  lazy val staged2direct =
    newPolyMethod(1, ScalaPackageClass, newTermName("staged2direct"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK.toStaged)), tpar.head.tpeHK))
  lazy val direct2staged =
    newPolyMethod(1, ScalaPackageClass, newTermName("direct2staged"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK.toStaged))
}