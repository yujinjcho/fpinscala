package problems.chapter4

object Chapter4 {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m.flatMap(xsMean => mean(xs.map(a => math.pow(a - xsMean, 2))) )
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)
}
