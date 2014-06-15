<img src="http://scala-miniboxing.org/mbox2-thumbnail.png" alt="Miniboxing Logo" align="right">

#Staging plugin [![Build Status](https://travis-ci.org/miniboxing/staging-plugin.svg?branch=master)](https://travis-ci.org/miniboxing/staging-plugin) 

Project Stagium (super-alpha) -- [**more info on the wiki**](https://github.com/miniboxing/staging-plugin/wiki)
```
sun@sun-laptop:/mnt/data-local/Work/Workspace/dev-2.11/staging-plugin/examples(master)$ cat stage.scala 
import stagium._
import scala.reflect.runtime.universe._

object Test {
  def main(args: Array[String]): Unit = {

    // this is a method with staged arguments
    def pow(e: Double @staged, p: Int): Double @staged =
      if ( p == 0 ) 1.0
      else if ( p % 2 == 1 ) e * pow(e, p - 1)
      else { // p % 2 == 0
        val x = pow(e, p/2)
        x * x
      }

    // and this is what we stage
    println("Result: " + execute(pow(3, 5)))
  }
}


// This is the support object for staging
object __staged {
  case class DoubleTimes(t1: Exp[Double], t2: Exp[Double]) extends Def[Double] {
    override def toString = t1 + " * " + t2
  }
  def infix_*(r: Exp[Double], oth: Exp[Double]): Exp[Double] = 
    (r, oth) match {
      case (Con(1), _) => oth
      case (_, Con(1)) => r
      case _ => addDef(DoubleTimes(r, oth))
    }
}

sun@sun-laptop:/mnt/data-local/Work/Workspace/dev-2.11/staging-plugin/examples(master)$ ../st-scalac stage.scala
sun@sun-laptop:/mnt/data-local/Work/Workspace/dev-2.11/staging-plugin/examples(master)$ ../st-scala Test
Need to compile and run:
*********************************
{
  val x0: Double = 3.0 * 3.0
  val x1: Double = x0 * x0
  val x2: Double = 3.0 * x1
  x2: Double
}
*********************************
Result: null

sun@sun-laptop:/mnt/data-local/Work/Workspace/dev-2.11/staging-plugin/examples(master)$ ../st-scalac stage.scala -P:stagium:passive
sun@sun-laptop:/mnt/data-local/Work/Workspace/dev-2.11/staging-plugin/examples(master)$ ../st-scala Test
Result: 243.0
```
