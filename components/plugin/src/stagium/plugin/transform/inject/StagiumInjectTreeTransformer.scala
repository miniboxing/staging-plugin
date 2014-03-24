package stagium.plugin
package transform
package inject

trait StagiumInjectTreeTransformer {
  this: StagiumInjectPhase =>

  import global._
  import definitions._
  import helper._

  class TreeInjector(unit: CompilationUnit) extends TreeRewriter(unit) {
    override def rewrite(tree: Tree)(implicit state: State) = {
      case tree @ ValDef(mods, name, tpt @ V(_), rhs) if afterInject(tree.symbol.info.isUnboxedStagiumRef) =>
        commit(treeCopy.ValDef(tree, mods, name, tpt.toUnboxedStagiumRef, rhs))
      case tree @ DefDef(mods, name, tparams, vparamss, tpt @ V(_), rhs) if !tree.symbol.isConstructor && afterInject(tree.symbol.info.finalResultType.isUnboxedStagiumRef) =>
        commit(treeCopy.DefDef(tree, mods, name, tparams, super.transformValDefss(vparamss), tpt.toUnboxedStagiumRef, rhs))
    }
  }
}