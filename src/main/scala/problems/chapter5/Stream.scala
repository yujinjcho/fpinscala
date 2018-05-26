package problems.chapter5

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case None => Nil
    case Some((h, t)) => List(h) ++: t.toList 
  }
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, t1))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
