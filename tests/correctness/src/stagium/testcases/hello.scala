package stagium
package examples
package hello

// This is our staging target
case class Hello(x: Int) {
  def +(hello: Hello) = Hello(this.x + hello.x)
  override def toString = s"stagium.examples.hello.Hello($x)"
}

// And this is our example of adding two hellos:
object Test {
  def main(args: Array[String]): Unit = {
    execute(new Hello(5) + new Hello(2))
  }
}

// This is the support object for staging:
object __staged {

  // this method represents the "alternative" staged +
  def infix_+(t1: Exp[Hello], t2: Exp[Hello]): Exp[Hello] =
    (t1, t2) match {
      // case (Con(h1), Con(h2)) => Con(Hello(h1.x + h2.x))
      case _ => HelloAdd(t1, t2)
    }

  // and this is its implementation in the next execution stage:
  case class HelloAdd(t1: Exp[Hello], t2: Exp[Hello]) extends Def[Hello] {
    override def toString = s"Hello($t1.x + $t2.x)"
  }
}

