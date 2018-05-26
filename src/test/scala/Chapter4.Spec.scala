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

    scenario("5 - implement sequence") {
      val a = List(Some(1), Some(2))
      val b = Chapter4.sequence(a)
      assert(b.getOrElse(List()) == List(1,2))
    }

    scenario("6 - implement traverse") {
      val a = List(Some(1), Some(2))
      val b = Chapter4.sequenceViaTraverse(a)
      assert(b.getOrElse(List()) == List(1,2))
    }
  }

  feature("The Either data type") {
    scenario("7 - implement map, flatMap, orElse, map2 that operate on Right") {
      val a = Right(1)
      // map
      val b = a.map(x => x + 1)
      b match {
        case Right(x) => assert(x == 2)
        case _ => assert(false)
      }

      // flatMap
      val c = a.flatMap(x => Right(x + 2))
      c match {
        case Right(x) => assert(x == 3)
        case _ => assert(false)
      }

      // orElse
      val d = a.orElse(Right(10))
      d match {
        case Right(x) => assert(x == 1)
        case _ => assert(false)
      }
    }
  }
}
