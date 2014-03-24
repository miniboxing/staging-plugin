package stagium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait StagiumDefs {
  this: StagiumHelper =>

  import global._
  import definitions._

  lazy val StagedClass = rootMirror.getRequiredClass("scala.staged")

  // artificially created marker methods
  lazy val unbox2box =
    newPolyMethod(1, ScalaPackageClass, newTermName("unbox2box"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK withAnnotation AnnotationInfo(StagedClass.tpe, Nil, Nil))), tpar.head.tpeHK))
  lazy val box2unbox =
    newPolyMethod(1, ScalaPackageClass, newTermName("box2unbox"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK withAnnotation AnnotationInfo(StagedClass.tpe, Nil, Nil)))
}