package problems.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2 - implement tail function for removing first element
  def tail[A](ds: List[A]): List[A] = ds match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  // Exercise 3 - generalize tail to drop n elements
  def drop[A](ds: List[A], n: Int): List[A] =
    if (n == 0) ds
    else ds match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }

  // Exercise 4 - Implement dropWhile
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  // Exercise 5 - Implement setHead
  def setHead[A](ds: List[A], a: A): List[A] = {
    ds match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }
  }

  // Exercise 6 - implement init which drops last item
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // Exercise 9 - compute lenght using foldRight
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 10: do a tail recursive foldLeft
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(x,z))(f)
    }

  // Exercise 11: sum, product, length with foldLeft
  def sumFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthFoldLeft(l: List[Int]): Int = foldLeft(l, 0)((x, acc) => acc + 1)

  // Exercise 12: reverse with fold
  def reverse[A](l: List[A]): List[A] =
     foldLeft(l, Nil: List[A])(Cons(_,_))

  // Exercise 13: implement foldLeft with foldRight
   def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
     foldRight(l, (b:B) => b)((a, g) => t => g(f(a, t)))(z)

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
     foldLeft(l, (b:B) => b)((a, g) => b => g(f(a, b)))(z)


  // Exercise 14: implement append via fold
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  // Exercise 15: implement function to concat list of list into single list
  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  // Exercise 18: implement map
  def map[A,B](l: List[A])(f: A => B): List[B] =
    List.foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  // Exercise 19: implement filter
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    List.foldRight(l, Nil: List[A])((x, acc) => {
      if (f(x)) Cons(x, acc) else acc
    })

  // Exercise 20: implement flatMap
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  // Exercise 21: filter via flatMap
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((x) => {
      if (f(x)) List(x) else Nil
    })

  // Exercise 23: zip and map
  def zipAndTransform[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] =
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipAndTransform(t1, t2)(f))
    }

}
