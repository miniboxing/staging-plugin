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
//        // remove redundant asInstanceOf-s introduced by tailcalls
//      case AsInstanceOf(expr, tpe) if expr.tpe =:= tpe.tpe && tpe.tpe.typeSymbol.isStagiumClass =>
//        stagiumlog("removed redundant asInstanceOf:")
//        stagiumlog("  tree: " + tree)
//        stagiumlog("  expr: " + tree)
//        stagiumlog("  tree.tpe: " + tree.tpe)
//        stagiumlog("  expr.tpe: " + expr.tpe)
//        stagiumlog("  tpe.tpe: " + tpe.tpe)
//        transform(expr)
      case _ =>
        super.transform(tree)
    }
  }
}