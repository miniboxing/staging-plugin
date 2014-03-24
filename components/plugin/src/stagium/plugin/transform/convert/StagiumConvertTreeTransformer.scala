package stagium.plugin
package transform
package convert

trait StagiumConvertTreeTransformer {
  this: StagiumConvertPhase =>

  import global._
  import definitions._
  import treeInfo.{AsInstanceOf => _, _}
  import helper._
  import Flag._

}