package problems.chapter4

import org.scalatest._

class Chapter4Spec extends FeatureSpec {
  feature("usage patterns for Option") {
    scenario("2 - implement variance") {
      val a = Seq(1.0,2.0,3.0)
      val v = Chapter4.variance(a)
      assert(v.getOrElse(0.0) > .666666)
      assert(v.getOrElse(0.0) < .67)
    }

    scenario("3 - implement map2") {
      val a = Some(1)
      val b = Some(2.0)
      val r = Chapter4.map2(a, b)((x,y) => x.toString + y.toString)
      assert(r.getOrElse("") == "12.0")
    }

    scenario("bothMatch is working") {
      val a = Chapter4.bothMatch("a", "a", "a")
      assert(a.getOrElse(false))
    }

    scenario("4 - implement bothMatch with map2") {
      val a = Chapter4.bothMatch2("a", "a", "a")
      assert(a.getOrElse(false))
    }
  }
}
