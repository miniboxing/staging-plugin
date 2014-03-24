package stagium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait StagiumDefs {
  this: StagiumHelper =>

  import global._
  import definitions._

  lazy val StagedClass = rootMirror.getRequiredClass("scala.staged")
  lazy val StagiumPackage = rootMirror.getPackage("stagium")

  lazy val ExpClass = rootMirror.getRequiredClass("stagium.Exp")
  lazy val ConClass = rootMirror.getRequiredClass("stagium.Con")

  lazy val unstageInterface = getMember(StagiumPackage, TermName("execute"))
  lazy val unstageImplment  = getMember(StagiumPackage, TermName("execute_impl"))

  // artificially created marker methods
  lazy val staged2direct =
    newPolyMethod(1, ScalaPackageClass, newTermName("staged2direct"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK.toStaged)), tpar.head.tpeHK))
  lazy val direct2staged =
    newPolyMethod(1, ScalaPackageClass, newTermName("direct2staged"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK.toStaged))
}