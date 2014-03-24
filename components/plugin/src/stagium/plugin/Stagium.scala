package stagium.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent

import transform._
import metadata._

/** Main miniboxing class */
class Stagium(val global: Global) extends Plugin { plugin =>
  // import global._

  val name = "stagium"
  val description = "provides value class functionality"

  val components = List[PluginComponent](
    StagiumPreparePhaseObj,
    StagiumVerifyPhaseObj,
    StagiumInjectPhaseObj,
    StagiumCoercePhaseObj,
    StagiumConvertPhaseObj,
    StagiumAddExtensionMethodsPhaseObj
  )

  // LDL adaptation
  global.addAnnotationChecker(StagiumCoercePhaseObj.ValueAnnotationChecker)

  lazy val helper = new { val global: plugin.global.type = plugin.global } with StagiumHelper

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      error("Stagium: option not understood: " + option)
    }
  }

  private object StagiumPreparePhaseObj extends StagiumPreparePhase { self =>
    val global: Stagium.this.global.type = Stagium.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("specialize")
    val phaseName = Stagium.this.name + "-prepare"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var stagiumPreparePhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      stagiumPreparePhase = new Phase(prev)
      stagiumPreparePhase
    }
  }

  private object StagiumVerifyPhaseObj extends StagiumVerifyPhase { self =>
    val global: Stagium.this.global.type = Stagium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(StagiumPreparePhaseObj.phaseName)
    val phaseName = Stagium.this.name + "-verify"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var stagiumVerifyPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      stagiumVerifyPhase = new Phase(prev)
      stagiumVerifyPhase
    }
  }

  private object StagiumInjectPhaseObj extends StagiumInjectPhase { self =>
    val global: Stagium.this.global.type = Stagium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(StagiumVerifyPhaseObj.phaseName)
    val phaseName = Stagium.this.name + "-inject"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var stagiumInjectPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      stagiumInjectPhase = new Phase(prev)
      stagiumInjectPhase
    }
  }

  private object StagiumCoercePhaseObj extends StagiumCoercePhase { self =>
    val global: Stagium.this.global.type = Stagium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(StagiumInjectPhaseObj.phaseName)
    val phaseName = Stagium.this.name + "-coerce"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var stagiumCoercePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      stagiumCoercePhase = new CoercePhase(prev)
      stagiumCoercePhase
    }
  }

  private object StagiumConvertPhaseObj extends StagiumConvertPhase { self =>
    val global: Stagium.this.global.type = Stagium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(StagiumCoercePhaseObj.phaseName)
    val phaseName = Stagium.this.name + "-convert"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var stagiumConvertPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      stagiumConvertPhase = new Phase(prev)
      stagiumConvertPhase
    }
  }

  private object StagiumAddExtensionMethodsPhaseObj extends StagiumAddExtensionMethodsPhase { self =>
    val global: Stagium.this.global.type = Stagium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(StagiumConvertPhaseObj.phaseName)
    val phaseName = Stagium.this.name + "-addext"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var stagiumAddExtPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      stagiumAddExtPhase = new Phase(prev)
      stagiumAddExtPhase
    }
  }
}
