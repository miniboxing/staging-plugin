package p

class ann extends annotation.StaticAnnotation with annotation.TypeConstraint

class Seq[T] {
  def map[U](f: T => U): Seq[U] = ???
}

object Test {
  def test() = {
    val seq1 = new Seq[Int @ann]
    val seq2 = seq1.map(x => x)
    //  ^               ^
    //  |               |
    //  |               correctly inferred to Int @ann => Int @ann
    //  |
    //  typer infers Seq[Int] instead of Seq[Int @ann]
  }
}
