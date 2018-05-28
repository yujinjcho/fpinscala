package problems.chapter5

import org.scalatest._

class Chapter5Spec extends FeatureSpec {
  feature("lazy lists") {
    scenario("1 - implement toList") {
      val s1: Stream[Int] = Stream(1,2,3)
      assert(s1.toList == List(1,2,3))
    }

    scenario("2 - implement take and skip") {
      val s1: Stream[Int] = Stream(1,2,3)
      assert(s1.take(1).toList == List(1))

      val s2: Stream[Int] = Stream(1,2,3)
      assert(s2.drop(1).toList == List(2,3))
    }

    scenario("3 - implement takeWhile") {
      val s1: Stream[Int] = Stream(1,1,2,3,1)
      assert(s1.takeWhile(_ == 1).toList == List(1,1))
    }

    scenario("4 - implement forAll") {
      val s1: Stream[Int] = Stream(1,1,2,1,1)
      assert(s1.forAll(_ == 1) == false)
    }

    scenario("5 - implement takeWhile with foldRight") {
      val s1: Stream[Int] = Stream(1,1,2,1,1)
      assert(s1.takeWhileViaFoldRight(_ == 1).toList == List(1,1))
    }

    scenario("6 - implement headOption with foldRight") {
      val s1: Stream[Int] = Stream(1,1,2,1,1)
      assert(s1.headOption == Some(1))
    }

    scenario("7 - implement map, filter, append, and flatMap using foldRight") {
      val s1: Stream[Int] = Stream(1,1,2)
      assert(s1.map(_ + 1).toList == List(2,2,3))
      assert(s1.filter(_ == 1).toList == List(1, 1))
      assert(s1.append(Stream(3)).toList == List(1,1,2,3))
      assert(s1.flatMap(x => Stream(x + 1)).toList == List(2,2,3))
      assert(s1.flatMap(x => Stream(x + 1, 10)).toList == List(2, 10, 2, 10, 3, 10))
    }

    scenario("8 - implement constant stream") {
      val s1: Stream[Int] = Stream.constant(2)
      assert(s1.take(2).toList == List(2,2))
    }

    scenario("9 - implement from") {
      val s1: Stream[Int] = Stream.from(10)
      assert(s1.take(2).toList == List(10,11))
    }

    scenario("10 - implement fib stream") {
      assert(Stream.fibs.take(6).toList == List(0,1,1,2,3,5))
    }
  }
}
