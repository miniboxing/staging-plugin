package stagium.plugin
package transform

import metadata._

import prepare._
import coerce._
import convert._

/** Removes the known problems in the Scala ASTs that cause the plugin
 *  to malfunction. For example: tailcall introduces .asInstancOf-s that
 *  prevent proper transformation and thus crash in the backend. */
trait StagiumPreparePhase extends
    StagiumPluginComponent
    with scala.tools.nsc.transform.Transform
    with StagiumPrepareTreeTransformer { self =>
  import global._
  def stagiumPreparePhase: StdPhase
  def afterPrepare[T](op: => T): T = global.exitingPhase(stagiumPreparePhase)(op)
  def beforePrepare[T](op: => T): T = global.enteringPhase(stagiumPreparePhase)(op)

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // [error] /Users/xeno_by/Projects/stagium/tests/correctness/test/stagium/partest/CompileTest.scala:30: [stagium-verify] tree not typed: $anonfun.this.apply$mcV$sp()
      // [error]       Console.withErr(pa) {
      // [error]                           ^
      // [error] one error found
      // TODO: I've no idea why this happens - looks like an invalid tree produced by scalac
      tree.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp")) unit.error(tree.pos, s"[stagium-verify] tree not typed: $tree"))
      new TreePreparer(unit).transform(tree)
    }
  }
}

/** Adds direct2staged and staged2direct coercions based on annotations injected during the previous phase */
trait StagiumCoercePhase extends
    StagiumPluginComponent
    with StagiumCoerceTreeTransformer
    with StagiumAnnotationCheckers { self =>
  import global._
  def stagiumCoercePhase: StdPhase
  def afterCoerce[T](op: => T): T = global.exitingPhase(stagiumCoercePhase)(op)
  def beforeCoerce[T](op: => T): T = global.enteringPhase(stagiumCoercePhase)(op)
}

/** Representation conversion phase `C @value -> fields` */
trait StagiumConvertPhase extends
    StagiumPluginComponent
    with StagiumConvertInfoTransformer
    with StagiumConvertTreeTransformer { self =>
  import global._
  import helper._
  def stagiumConvertPhase: StdPhase
  def afterConvert[T](op: => T): T = global.exitingPhase(stagiumConvertPhase)(op)
  def beforeConvert[T](op: => T): T = global.enteringPhase(stagiumConvertPhase)(op)
}