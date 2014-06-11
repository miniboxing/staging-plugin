package stagium
package examples
package pow

import scala.reflect.runtime.universe._

// A test for the power function
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
    println("execute: " + execute(pow(3, 5)) + "\n")
    val fun1 = function1[Double, Double](e => pow(e, 5))
    println("fun1(3): " + fun1(3) + "\n")
    val fun2 = function2[Double, Double, Double]((e1, e2) => pow(e1, 5) * pow(e2, 5))
    println("fun2(3, 1): " + fun2(3, 1) + "\n")
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
      case _ => DoubleTimes(r, oth)
    }
}


