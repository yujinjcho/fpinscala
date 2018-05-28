package problems.chapter5

import Stream._

trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = {
    if (n == 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }
  }

  def takeViaUnfold(n: Int) : Stream[A] = {
    unfold((this, n)) {
      case (Cons(h,t), 1) => Some((h(), (empty, 0)))
      case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h,t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  // def exists(p: A => Boolean): Boolean = this match {
  //   case Cons(h ,t) => p(h()) || t().exists(p)
  //   case _ => false
  // }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h ,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a,b) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f:A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapViaUnfold[B](f:A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def filter(p:A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](x: => Stream[B]): Stream[B] =
    foldRight(x)((a, b) => cons(a, b))

  def flatMap[B](f:A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2,t2)) => Some(((None, Some(h2())), (empty, t2())))
      case (Cons(h1,t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipWith(s)((_,_)).forAll { case (a, b) => a == b }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h,t) => Some(t(), t())
      case _ => None
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(a: Int, b:Int): Stream[Int] = {
      val c = a + b
      cons(c ,loop(b, c))
    }
    Stream(0, 1) append loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some((a,s)) => cons(a, unfold(s)(f))
    }
  }

  def onesViaUnfold: Stream[Int] =
    unfold(1)(x => Some(x, x))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, x))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x+1))

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (a: Int, b: Int) => Some((a, (b, a + b)))}
}
