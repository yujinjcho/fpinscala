package problems.chapter3

import org.scalatest._

class Chapter3Spec extends FeatureSpec {
  feature("functional data structures and data sharing") {
    scenario("2 - implement tail function for removing first element") {
      val a = List(1,2,3)
      assert(List.tail(a) == List(2,3))
    }

    scenario("3 - generalize tail to drop n elements") {
      val a = List(1,2,3)
      assert(List.drop(a, 2) == List(3))
    }

    scenario("4 - implement dropWhile") {
      val a = List(1,2,3,4,5)
      assert(List.dropWhile(a)(_ < 3) == List(3,4,5))
    }

    scenario("5 - implement setHead") {
      val a = List(1,2)
      assert(List.setHead(a, 0) == List(0,2))
    }

    scenario("6 - implement init which drops last item") {
      val a = List(1,2,3)
      assert(List.init(a) == List(1,2))
    }

    scenario("9 - implement length using foldRight") {
      val a = List(1,2,3)
      assert(List.length(a) == 3)
    }
  }

  feature("recursion and generalizing to higher order functions") {
    scenario("10 - compute length of list using foldRight") {
      val l = List(1,2,3)
      val length = List.foldRight(l, 0)((_, acc) => acc + 1)
      assert(length == 3)
    }

    scenario("11 - write sum, product, length using foldLeft") {
      val l = List(4,4,4)
      assert(List.sumFoldLeft(l) == 12)
      assert(List.lengthFoldLeft(l) == 3)

      val l2 = List(4.0, 4.0, 4.0)
      assert(List.productFoldLeft(l2) == 64)
    }

    scenario("12 - write reverse using fold") {
      val l = List(1,2,3)
      assert(List.reverse(l) == List(3,2,1))
    }

    scenario("13 - write foldLeft in terms of foldRight and vice versa") {
      val l = List(1,2,3)
      val reversedL = List.foldLeftViaFoldRight(l, Nil:List[Int])(Cons(_,_))
      val notReversed = List.foldRightViaFoldLeft(l, Nil:List[Int])(Cons(_,_))
      assert(reversedL == List(3,2,1))
      assert(notReversed == l)
    }

    scenario("14 - implement append via fold") {
      val l = List.appendViaFoldRight(List(1,2), List(3,4))
      assert(l == List(1,2,3,4))
    }

    scenario("15 - implement func to concat list of list into single list") {
      val l = List.flatten(List(List(1,2), List(3,4), List(5)))
      assert(l == List(1,2,3,4,5))
    }
  }

  feature("more functions for working with lists") {
    scenario("16 - add 1 to each List[Int]") {
      def addOne(l: List[Int]): List[Int] =
        List.foldRight(l, Nil:List[Int])((x, acc) => Cons(x + 1,acc))
      val l = List(1,2,3)
      assert(addOne(l) == List(2,3,4))
    }

    scenario("17 - List[Double] to List[String]") {
      def doubleToString(l: List[Double]): List[String] =
        List.foldRight(l, Nil:List[String])((x, acc) => Cons(x.toString,acc))
      val l = List(1.0,2.0)
      assert(doubleToString(l) == List("1.0", "2.0"))
    }

    scenario("18 - implement map") {
      val l = List(1.0,2.0)
      val lStringified = List.map(l)((a) => a.toString)
      assert(lStringified == List("1.0", "2.0"))
    }

    scenario("19 - implement filter") {
      val l = List(1,2,3,4)
      val evenOnly = List.filter(l)(_ % 2 != 1)
      assert(evenOnly == List(2,4))
    }

    scenario("20 - implement flatMap") {
      val a = List.flatMap(List(1,2,3))(i => List(i,i))
      assert(a == List(1,1,2,2,3,3))
    }

    scenario("21 - implement filter via flatMap") {
      val l = List(1,2,3,4)
      val evenOnly = List.filterViaFlatMap(l)(_ % 2 != 1)
      assert(evenOnly == List(2,4))
    }

    scenario("22 - accepts two list and adds them") {
      def addLists(l1: List[Int], l2: List[Int]): List[Int] =
        (l1, l2) match {
          case (_, Nil) => Nil
          case (Nil, _) => Nil
          case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
        }

      val r = addLists(List(1,2), List(3,4))
      assert(r == List(4,6))
    }

    scenario("23 - zip and map") {
      val r = List.zipAndTransform(List(1,2), List(3,4))(_ + _)
      assert(r == List(4,6))
    }

    scenario("24 - implement has subsequence") {
      def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
        (l, sub) match {
          case (_, Nil) => true
          case (Nil, _) => false
          case (Cons(h1, t1), Cons(h2, t2)) => {
            if (h1 == h2) hasSubsequence(t1, t2)
            else hasSubsequence(t1, sub)
          }
        }

      val l = List(1,2,3,4)
      val sub = List(3,4)
      assert(hasSubsequence(l, sub))
      assert(!hasSubsequence(l, List(5)))
    }
  }

  feature("Trees") {
    scenario("25 - write function size that counts nodes") {
      def size[A](t:Tree[A]): Int =
        t match {
          case Leaf(_) => 1
          case Branch(left, right) => size(left) + size(right) + 1
        }
      val b1 = Branch(Leaf(1), Leaf(2))
      assert(size(b1) == 3)
    }

    scenario("26 - write func to return max element in tree") {
      def max(t:Tree[Int]): Int =
        t match {
          case Leaf(value) => value
          case Branch(left, right) => max(left).max(max(right))
        }
      val b1 = Branch(Leaf(1), Leaf(2))
      assert(max(b1) == 2)
    }

    scenario("27 - write func to return depth") {
      def depth[A](t:Tree[A]): Int =
        t match {
          case Leaf(_) => 0
          case Branch(left, right) => depth(left).max(depth(right)) + 1
        }
      val b1 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(4))
      assert(depth(b1) == 2)
    }
  }
}
