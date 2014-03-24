package stagium.plugin
package transform
package verify

import scala.reflect.internal.Flags._

trait StagiumVerifyTreeTransformer {
  this: StagiumVerifyPhase =>

  import global._
  import definitions._
  import helper._

  class TreeVerifier(unit: CompilationUnit) extends Traverser {
    override def traverse(tree: Tree): Unit = tree match {
      case ClassDef(_, _, tparams, Template(_, _, stats)) if tree.symbol.isStagiumClass =>
        if (tree.symbol.isAbstract) unit.error(tree.pos, "`abstract' modifier cannot be used with stagium classes")
        tree.symbol.setFlag(FINAL)
        val constrParamAccessors = tree.symbol.constrParamAccessors.map(field => (field, field.getterIn(field.owner), field.setter(field.owner)))
        constrParamAccessors collect { case (field, getter, _) if getter == NoSymbol || !getter.isPublic => unit.error(field.pos, "there can only be public fields in stagium classes") }
        constrParamAccessors collect { case (field, _, setter) if setter != NoSymbol => unit.error(field.pos, "there can only be immutable fields in stagium classes") }
        constrParamAccessors collect { case (field, _, _) if field.info.typeSymbol.isStagiumClass => unit.error(field.pos, "there can only be non-stagiumclass fields in stagium classes") }
        stats collect {
          // TODO: this is to avoid dealing with types dependent on stagium classes
          // those can be supported in stagium-convert by just replacing p.T's to their upper bounds
          // (that's valid, because stagium classes are final, and because typechecker has already checked that path-dependent types are ok)
          // however that would be tedious in the sense that we need to make these replacements everywhere - in trees, in symbol signatures, etc
          case tdef: TypeDef => unit.error(tdef.pos, "type members can't be defined in stagium classes")
          case vdef: ValDef if !vdef.symbol.isParamAccessor => unit.error(vdef.pos, "additional parameters can't be defined in stagium classes")
          case ddef: DefDef if ddef.symbol.isAuxiliaryConstructor => unit.error(ddef.pos, "secondary constructors can't be defined in stagium classes")
          // TODO: automatically synthesize implementations of equals and hashcode
          // case ddef: DefDef if ddef.name == nme.equals_ || ddef.name == nme.hashCode_ && !ddef.symbol.isSynthetic => unit.error(ddef.pos, "equals and hashCode can't be defined in stagium classes")
          case ddef: DefDef => // legal
          case stat: Import => // legal
          case EmptyTree => // legal
          case stat if stat.symbol.isParamAccessor => // legal
          case stat => unit.error(stat.pos, "invalid statement in a stagium class")
        }
        super.traverse(tree)
      case _: MemberDef if tree.symbol.isStagiumClass =>
        unit.error(tree.pos, "only classes (not traits) are allowed to be @stagium")
        super.traverse(tree)
      case _: ImplDef if tree.symbol.info.parents.exists(_.typeSymbol.isStagiumClass) =>
        unit.error(tree.pos, "can't inherit from a stagium class")
        super.traverse(tree)
      case _ =>
        // TODO: need to ban p.type for stagium classes
        super.traverse(tree)
    }
  }
}