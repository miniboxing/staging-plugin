//
// This file was taken from the Rosetta Code project
// the code is under the GNU Free Documentation License 1.2
// http://www.gnu.org/licenses/fdl-1.2.html
//
// original URL: http://rosettacode.org/wiki/Fast_Fourier_transform#Scala
//
package stagium.bench.complex.fft

import scala.math.{ Pi, cos, sin, cosh, sinh, abs }
import stagium._
import scala.reflect.runtime.universe._

case class Complex(re: Double, im: Double) {
    def +(x: Complex @staged): Complex @staged = ??? //Complex(re + x.re, im + x.im)
    def -(x: Complex @staged): Complex @staged = ??? //Complex(re - x.re, im - x.im)
    def **(x: Double @staged): Complex @staged = ??? //Complex(re * x, im * x)
    def *(x: Complex @staged): Complex @staged = ??? //Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    def /(x: Double @staged):  Complex @staged = ??? //Complex(re / x, im / x)

    override def toString(): String = {
        val a = "%1.3f" format re
        val b = "%1.3f" format abs(im)
        (a,b) match {
            case (_, "0.000") => a
            case ("0.000", _) => b + "i"
            case (_, _) if im > 0 => a + " + " + b + "i"
            case (_, _) => a + " - " + b + "i"
        }
    }
}

object Complex {
   def exp(c: Complex @staged): Complex @staged = ???
//  def exp(c: Complex) : Complex = {
//      val r = (cosh(c.re) + sinh(c.re))
//      Complex(cos(c.im), sin(c.im)) * r
//  }
}

object FFT {
  def _fft(cSeq: Seq[Complex @staged], direction: Complex, scalar: Int): Seq[Complex @staged] = {
    if (cSeq.length == 1) {
        cSeq
    } else {
        val n = cSeq.length
        assume(n % 2 == 0, "The Cooley-Tukey FFT algorithm only works when the length of the input is a power of two.")

        val evenOddPairs = cSeq.grouped(2).toSeq
        val evens = _fft(evenOddPairs map (_(0)), direction, scalar)
        val odds  = _fft(evenOddPairs map (_(1)), direction, scalar)

        def leftRightPair(k: Int): (Complex @staged, Complex @staged) = {
            val base = evens(k) / scalar
            val offset = odds(k) * Complex.exp(direction ** (Pi * k / n)) / scalar
            (base + offset, base - offset)
        }

        val pairs = (0 until n/2) map leftRightPair
        val left  = pairs map (_._1)
        val right = pairs map (_._2)
        left ++ right
    }
  }

  def  fft(cSeq: Seq[Complex @staged]): Seq[Complex @staged] = _fft(cSeq, Complex(0,  2), 1)
  def rfft(cSeq: Seq[Complex @staged]): Seq[Complex @staged] = _fft(cSeq, Complex(0, -2), 2)
}


//
// This is the support object for staging
//
object __staged {
  case class Add[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " + " + t2 }
  case class Sub[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " - " + t2 }
  case class Mul[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " * " + t2 }
  case class Div[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " / " + t2 }

  def infix_+(t1: Exp[Complex], t2: Exp[Complex]): Exp[Complex] =
    (t1, t2) match {
      case _ => addDef(Add(t1, t2))
    }

  def infix_-(t1: Exp[Complex], t2: Exp[Complex]): Exp[Complex] =
    (t1, t2) match {
      case _ => addDef(Sub(t1, t2))
    }

  def infix_*(t1: Exp[Complex], t2: Exp[Complex]): Exp[Complex] =
    (t1, t2) match {
      case _ => addDef(Mul(t1, t2))
    }

  def infix_**(t1: Exp[Complex], t2: Exp[Double]): Exp[Complex] =
    (t1, t2) match {
      case _ => addDef(Mul(t1, t2))
    }

 def infix_/(t1: Exp[Complex], t2: Exp[Double]): Exp[Complex] =
    (t1, t2) match {
      case _ => addDef(Div(t1, t2))
    }
}

