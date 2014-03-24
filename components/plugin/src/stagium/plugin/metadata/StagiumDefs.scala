package stagium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait StagiumDefs {
  this: StagiumHelper =>

  import global._
  import definitions._

  lazy val StagiumClass = rootMirror.getRequiredClass("scala.stagium")

  /**
   * This class should only appear in the tree starting from the `stagium-inject` phase
   * and should be cleaned up afterwards, during the `stagium-coerce` phase.
   */
  lazy val UnboxedClass = {
    // This is what is should look like:
    // ```
    //   package __root__.scala {
    //     class unboxed extends Annotation with TypeConstraint
    //   }
    // ```
    val UnboxedSym = ScalaPackageClass.newClassSymbol(TypeName("unboxed"), NoPosition, 0L)
    UnboxedSym setInfoAndEnter ClassInfoType(List(AnnotationClass.tpe, TypeConstraintClass.tpe), newScope, UnboxedSym)
    UnboxedSym
  }

  // artificially created marker methods
  lazy val unbox2box =
    newPolyMethod(1, ScalaPackageClass, newTermName("unbox2box"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK withAnnotation AnnotationInfo(UnboxedClass.tpe, Nil, Nil))), tpar.head.tpeHK))
  lazy val box2unbox =
    newPolyMethod(1, ScalaPackageClass, newTermName("box2unbox"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK withAnnotation AnnotationInfo(UnboxedClass.tpe, Nil, Nil)))
}