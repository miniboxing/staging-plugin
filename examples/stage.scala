import stagium._
import scala.reflect.runtime.universe._
//class staged extends annotation.StaticAnnotation

object Test {

  implicit object ArithStager extends Stager {
    def stage[T: TypeTag](x: Exp[T]): T = ???
  } 

  def main(args: Array[String]): Unit = {

    def pow(e: Double @staged, p: Int): Double =
      if ( p == 0 ) 1.0
      else if ( p % 2 == 1 ) e * pow(e, p - 1)
      else { // p % 2 == 0
        val x = pow(e, p/2)
        x * x
      }

    unstage(pow(3, 7))
  }
}
