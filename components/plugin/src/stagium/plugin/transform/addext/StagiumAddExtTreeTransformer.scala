package stagium.plugin
package transform
package addext

trait StagiumAddExtTreeTransformer {
  this: StagiumAddExtensionMethodsPhase =>

  import global._
  import definitions._

  class TreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree
  }
}