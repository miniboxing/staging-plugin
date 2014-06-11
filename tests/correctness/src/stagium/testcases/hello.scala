package stagium
package examples
package hello

class Hello {
  def +(hello: Hello) = hello
}

object Test {
  def boo(hello: Hello @staged): Hello @staged = {
    hello + hello
  }
}

// This is the support object for staging
object __staged {
  def infix_+(t1: Exp[Hello], t2: Exp[Hello])(implicit ev: Hello <:< AnyRef): Hello @staged =
    ???
}

