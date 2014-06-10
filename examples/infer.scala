

class Seq[T] {
  def map[U](f: T => U): Seq[U] = ???
}

object Test {
  def test() = {
    val seq1 = new Seq[Double @staged]
    val seq2 = seq1.map(x => x)
  }
}
