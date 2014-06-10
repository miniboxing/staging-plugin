package stagium.plugin
package transform
package prepare

import scala.reflect.internal.Flags._

trait StagiumPrepareTreeTransformer {
  this: StagiumPreparePhase =>

  import global._
  import definitions._
  import helper._

  class TreePreparer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      case _ =>
        super.transform(tree)
    }
  }
}