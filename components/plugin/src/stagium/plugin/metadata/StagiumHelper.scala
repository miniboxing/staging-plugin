package stagium.plugin.metadata

import scala.tools.nsc.Global

trait StagiumHelper extends StagiumInfo with StagiumDefs {
  val global: Global
}
