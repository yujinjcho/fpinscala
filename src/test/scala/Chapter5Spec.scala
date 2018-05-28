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

    scenario("11 - implement unfold") {
      val s: Stream[Int] = Stream.unfold(1)(x => Some(x, x*10))
      assert(s.take(3).toList == List(1, 10, 100))
    }

    scenario("12 - implement fibs, from, constant, and ones with unfold") {
      assert(Stream.onesViaUnfold.take(3).toList == List(1,1,1))
      assert(Stream.constantViaUnfold(3).take(3).toList == List(3,3,3))
      assert(Stream.fromViaUnfold(3).take(3).toList == List(3,4,5))
      assert(Stream.fibsViaUnfold.take(6).toList == List(0,1,1,2,3,5))
    }

    scenario("13 - implement map, take, takeWhile, zipWith with unfold") {
      val s: Stream[Int] = Stream(1,2,3)
      val s2: Stream[Int] = Stream(1,1,1)
      val s3: Stream[Int] = Stream(1,1,1,1)
      assert(s.mapViaUnfold(_ + 1).toList == List(2,3,4))
      assert(s.takeViaUnfold(1).toList == List(1))
      assert(s.takeWhileViaFoldRight(_ < 3).toList == List(1,2))
      assert(s.zipWith(s2)(_ + _).toList == List(2,3,4))
      assert(s2.zipAll(s3).toList == List((Some(1),Some(1)), (Some(1),Some(1)), (Some(1),Some(1)), (None,Some(1))))
    }

    scenario("14 - implement startsWith") {
      val s1: Stream[Int] = Stream(1,2,3)
      val s2: Stream[Int] = Stream(1)
      val s3: Stream[Int] = Stream(1,2,3)
      val s4: Stream[Int] = Stream(1,3)
      assert(s1.startsWith(s2))
      assert(!s1.startsWith(s4))
    }

    scenario("15 - implement tails") {
      val s1: Stream[Int] = Stream(1,2)
      assert(s1.tails.map(_.toList).toList == List(List(2), List()))
    }


    scenario("16 - implement scanRight") {
      assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6,5,3,0))
    }
  }
}
