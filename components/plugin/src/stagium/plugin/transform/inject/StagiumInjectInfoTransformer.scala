package stagium.plugin
package transform
package inject

import scala.tools.nsc.transform.InfoTransform

trait StagiumInjectInfoTransformer extends InfoTransform {
  self: StagiumInjectPhase =>

  import global._
  import definitions._
  import helper._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    // need to transform:
    // 1) def foo(x: C) = ??? => def foo(x: C @value) = ???
    // 2) val x: C = ???      => val x: C @value = ???
    // 3) def foo: C = ???    => def foo: C @value = ??? (only for 1-arg value classes)
    def logTransform(tpe1: Type): Type = { stagiumlog(s"$sym: $tpe -> $tpe1"); tpe1 }
    if (sym.isTerm && sym.isParameter && tpe.isBoxedStagiumRef)
      logTransform(tpe.toUnboxedStagiumRef)
    else if (sym.isTerm && !sym.isMethod && tpe.isBoxedStagiumRef)
      logTransform(tpe.toUnboxedStagiumRef)
    else if (sym.isMethod && !sym.isConstructor && tpe.finalResultType.isBoxedStagiumRef && tpe.finalResultType.stagiumFields.length == 1 && !sym.isInjected)
      logTransform(tpe.toUnboxedStagiumRef)
    else
      tpe
  }
}
