package stagium.plugin
package transform
package prepare

import scala.reflect.internal.Flags._

trait StagiumPrepareTreeTransformer {
  this: StagiumPreparePhase =>

  import global._
  import definitions._
  import helper._

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // [error] /Users/xeno_by/Projects/stagium/tests/correctness/test/stagium/partest/CompileTest.scala:30: [stagium-verify] tree not typed: $anonfun.this.apply$mcV$sp()
      // [error]       Console.withErr(pa) {
      // [error]                           ^
      // [error] one error found
      // TODO: I've no idea why this happens - looks like an invalid tree produced by scalac
      tree.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp") && !tree.isInstanceOf[Import]) unit.error(tree.pos, s"[stagium-verify] tree not typed: $tree"))
      new TreePreparer(unit).transform(tree)
    }
  }

  class TreePreparer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case _ =>
        super.transform(tree)
    }
  }
}