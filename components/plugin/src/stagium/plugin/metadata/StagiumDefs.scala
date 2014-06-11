package stagium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait StagiumDefs {
  this: StagiumHelper =>

  import global._
  import definitions._

  lazy val StagedClass     = rootMirror.getRequiredClass("scala.staged")
  lazy val StagiumPackage  = rootMirror.getPackage("stagium")
  lazy val StagiumInternal = rootMirror.getRequiredModule("stagium.internal")

  lazy val ExpClass = rootMirror.getRequiredClass("stagium.Exp")
  lazy val ConClass = rootMirror.getRequiredClass("stagium.Con")

  lazy val executeInterface = getMember(StagiumPackage, TermName("execute"))
  lazy val executeImplement = getMember(StagiumInternal, TermName("execute_impl"))

  lazy val function1Interface = getMember(StagiumPackage, TermName("function1"))
  lazy val function2Interface = getMember(StagiumPackage, TermName("function2"))
  lazy val function4Interface = getMember(StagiumPackage, TermName("function4"))
  lazy val function8Interface = getMember(StagiumPackage, TermName("function8"))
  lazy val function1Implement = getMember(StagiumInternal, TermName("function1_impl"))
  lazy val function2Implement = getMember(StagiumInternal, TermName("function2_impl"))
  lazy val function4Implement = getMember(StagiumInternal, TermName("function4_impl"))
  lazy val function8Implement = getMember(StagiumInternal, TermName("function8_impl"))

  lazy val functionInterfaces = List(function1Interface, function2Interface, function4Interface, function8Interface)
  lazy val functionImplements = Map(
      function1Interface -> function1Implement,
      function2Interface -> function2Implement,
      function4Interface -> function4Implement,
      function8Interface -> function8Implement
  )

  // artificially created marker methods
  lazy val staged2direct =
    newPolyMethod(1, ScalaPackageClass, newTermName("staged2direct"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK.toStaged)), tpar.head.tpeHK))
  lazy val direct2staged =
    newPolyMethod(1, ScalaPackageClass, newTermName("direct2staged"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK.toStaged))
}