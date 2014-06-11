//
// This file was taken from the Rosetta Code project
// the code is under the GNU Free Documentation License 1.2
// http://www.gnu.org/licenses/fdl-1.2.html
//
// original URL: http://rosettacode.org/wiki/Fast_Fourier_transform#Scala
//
package stagium
package examples
package fft

import scala.math.{ Pi, cos, sin, cosh, sinh, abs }
import scala.reflect.runtime.universe._

case class Complex(re: Double, im: Double) {
    def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
    def -(x: Complex): Complex = Complex(re - x.re, im - x.im)
    def **(x: Double): Complex = Complex(re * x, im * x)
    def *(x: Complex): Complex = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    def /(x: Double):  Complex = Complex(re / x, im / x)

    override def toString(): String = s"stagium.examples.fft.Complex($re, $im)"
    def exp: Complex = Complex.exp(this)
}

object Complex {
  def exp(c: Complex) : Complex = {
      val r = (cosh(c.re) + sinh(c.re))
      Complex(cos(c.im), sin(c.im)) ** r
  }
}

// annotations need to be added since the type inference does not kick in properly when annotations are present
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

// Test for staging
object Test {
  def main(args: Array[String]): Unit = {
    import FFT._

    val fun = function8((e1: Complex @staged, e2: Complex @staged, e3: Complex @staged, e4: Complex @staged, e5: Complex @staged, e6: Complex @staged, e7: Complex @staged, e8: Complex @staged) =>
                rfft(fft(Seq[Complex @staged](e1, e2, e3, e4, e5, e6, e7, e8)))(0))

    val c = Complex(1.0, 1.0)
    fun(c,c,c,c,c,c,c,c)
  }
}


// This is the support object for staging
// here we defined how to stage the operations for Complex numbers
object __staged {
  case class Re(c: Exp[Complex])  extends Def[Double]  { override def toString = s"$c.re" }
  case class Im(c: Exp[Complex])  extends Def[Double]  { override def toString = s"$c.im" }
  case class Add[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " + " + t2 }
  case class Sub[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " - " + t2 }
  case class Mul[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " * " + t2 }
  case class Div[T: TypeTag, U: TypeTag](t1: Exp[T], t2: Exp[U]) extends Def[T] { override def toString = t1 + " / " + t2 }
  case class E_^(c: Exp[Complex]) extends Def[Complex] { override def toString = "stagium.bench.complex.fft.Complex.exp(" + c + ")" }
  case class ToD(i: Exp[Int])     extends Def[Double]  { override def toString = i + ".toDouble" }

  def infix_re(c: Exp[Complex]): Exp[Double] =
    c match {
      case _ => Re(c)
    }

  def infix_im(c: Exp[Complex]): Exp[Double] =
    c match {
      case _ => Im(c)
    }

  def infix_+(t1: Exp[Complex], t2: Exp[Complex]): Exp[Complex] =
    (t1, t2) match {
      case _ => Add(t1, t2)
    }

  def infix_-(t1: Exp[Complex], t2: Exp[Complex]): Exp[Complex] =
    (t1, t2) match {
      case _ => Sub(t1, t2)
    }

  def infix_*(t1: Exp[Complex], t2: Exp[Complex]): Exp[Complex] =
    (t1, t2) match {
      case _ => Mul(t1, t2)
    }

  def infix_**(t1: Exp[Complex], t2: Exp[Double]): Exp[Complex] =
    (t1, t2) match {
      case _ => Mul(t1, t2)
    }

  def infix_/(t1: Exp[Complex], t2: Exp[Double]): Exp[Complex] =
    (t1, t2) match {
      case _ => Div(t1, t2)
    }

  def infix_exp(c: Exp[Complex]): Exp[Complex] =
    c match {
      case _ => E_^(c)
    }

  def infix_toDouble(i: Exp[Int]): Exp[Double] =
    i match {
      case _ => ToD(i)
    }
}
