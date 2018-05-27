package problems.chapter4

import org.scalatest._

class Chapter5Spec extends FeatureSpec {
  feature("lazy lists") {
    scenario("1 - implement toList") {
      val s1: Stream[Int] = Stream(1,2,3)
      assert(s1.toList == List(1,2,3))
    }

    scenario("2 - implement take") {
      // val s1: Stream[Int] = Stream(1,2,3)
      // assert(s1.take(1) == Stream(1))
    }
  }
}
