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
    def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
    def -(x: Complex): Complex = Complex(re - x.re, im - x.im)
    def **(x: Double): Complex = Complex(re * x, im * x)
    def *(x: Complex): Complex = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    def /(x: Double):  Complex = Complex(re / x, im / x)

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
    def exp: Complex = Complex.exp(this)
}

object Complex {
  def exp(c: Complex) : Complex = {
      val r = (cosh(c.re) + sinh(c.re))
      Complex(cos(c.im), sin(c.im)) ** r
  }
}

// annotations need to be added since the type inference does not kick in properly
// see https://groups.google.com/forum/#!topic/scala-internals/-hp79CrjQPo for more details
object FFT {
  def _fft(cSeq: Seq[Complex @staged], direction: Complex @staged, scalar: Int @staged): Seq[Complex @staged] = {
    if (cSeq.length == 1) {
        cSeq
    } else {
        val n = cSeq.length
        assume(n % 2 == 0, "The Cooley-Tukey FFT algorithm only works when the length of the input is a power of two.")

        val evenOddPairs = cSeq.grouped(2).toSeq
        val evens = _fft(evenOddPairs map[Complex @staged, Seq[Complex @staged]] (_(0)), direction, scalar)
        val odds  = _fft(evenOddPairs map[Complex @staged, Seq[Complex @staged]] (_(1)), direction, scalar)

        def leftRightPair(k: Int): (Complex @staged, Complex @staged) = {
            val base: Complex @staged   = evens(k) / scalar
            val offset: Complex @staged = odds(k) * ((direction ** (Pi * k / n)).exp / scalar)
            Tuple2[Complex @staged, Complex @staged](base + offset, base - offset)
        }

        val pairs = (0 until n/2) map leftRightPair
        val left  = pairs map[Complex @staged, Seq[Complex @staged]] (_._1)
        val right = pairs map[Complex @staged, Seq[Complex @staged]] (_._2)
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
  case class E_^(c: Exp[Complex]) extends Def[Complex] { override def toString = "stagium.bench.complex.fft.Complex.exp(" + c + ")" }
  case class ToD(i: Exp[Int])     extends Def[Double]  { override def toString = i + ".toDouble" }

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

  def infix_exp(c: Exp[Complex]): Exp[Complex] =
    addDef(E_^(c))

  def infix_toDouble(i: Exp[Int]): Exp[Double] =
    addDef(ToD(i))
}
