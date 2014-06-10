package stagium.plugin
package transform

import metadata._
import prepare._
import coerce._
import commit._

/** Removes the known problems in the Scala ASTs that cause the plugin
 *  to malfunction. For example: tailcall introduces .asInstancOf-s that
 *  prevent proper transformation and thus crash in the backend. */
trait StagiumPreparePhase extends
    StagiumPluginComponent
    with StagiumPrepareInfoTransformer
    with StagiumPrepareTreeTransformer { self =>
  import global._
  def stagiumPreparePhase: StdPhase
  def afterPrepare[T](op: => T): T = global.exitingPhase(stagiumPreparePhase)(op)
  def beforePrepare[T](op: => T): T = global.enteringPhase(stagiumPreparePhase)(op)
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
trait StagiumCommitPhase extends
    StagiumPluginComponent
    with StagiumCommitInfoTransformer
    with StagiumCommitTreeTransformer { self =>
  import global._
  import helper._
  def stagiumCommitPhase: StdPhase
  def afterCommit[T](op: => T): T = global.exitingPhase(stagiumCommitPhase)(op)
  def beforeCommit[T](op: => T): T = global.enteringPhase(stagiumCommitPhase)(op)
}