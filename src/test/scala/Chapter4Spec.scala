package problems.chapter4

import scala.{Option => _, Either => _, _}
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
      val a: Either[String,Int] = Right(1)
      val z: Either[String,Int] = Left("fail")
      // map
      val b = a.map(x => x + 1)
      b match {
        case Right(x) => assert(x == 2)
        case _ => assert(false)
      }

      val bLeft = z.map(x => x + 1)
      bLeft match {
        case Left(x) => assert(x == "fail")
        case _ => assert(false)
      }

      // flatMap
      val c = a.flatMap(x => Right(x + 2))
      c match {
        case Right(x) => assert(x == 3)
        case _ => assert(false)
      }

      val cLeft = z.flatMap(x => Right(x + 2))
      cLeft match {
        case Left(x) => assert(x == "fail")
        case _ => assert(false)
      }

      // orElse
      val d = a.orElse(Right(10))
      d match {
        case Right(x) => assert(x == 1)
        case _ => assert(false)
      }
      val dLeft = z.orElse(Right(20))
      dLeft match {
        case Right(x) => assert(x == 20)
        case _ => assert(false)
      }

      // map2
      val e = a.map2(Right(10))((x,y) => x*y)
      e match {
        case Right(x) => assert(x == 10)
        case _ => assert(false)
      }
      val eLeft = z.map2(Right(10))((x,y) => x*y)
      eLeft match {
        case Left(x) => assert(x == "fail")
        case _ => assert(false)
      }
      val eLeft2 = a.map2(z)((x,y) => x*y)
      eLeft match {
        case Left(x) => assert(x == "fail")
        case _ => assert(false)
      }
    }

    scenario("8 - implement sequence and traverse") {
      val a: Either[String,Int] = Right(1)
      val b: Either[String,Int] = Right(2)
      val z: Either[String,Int] = Left("fail")
      val es1 = List(a,b)
      val r1 = Either.sequence(es1)
      r1 match {
        case Right(x) => assert(x == List(1,2))
        case _ => assert(false)
      }

      val es2 = List(a,b,z)
      val r2 = Either.sequence(es2)
      r2 match {
        case Left(x) => assert(x == "fail")
        case _ => assert(false)
      }

      val t1 = Either.traverse(es1)(x => x.map(_ + 1))
      t1 match {
        case Right(x) => assert(x == List(2,3))
        case _ => assert(false)
      }

      val t2 = Either.traverse(es2)(x => x.map(_ + 1))
      t2 match {
        case Left(x) => assert(x == "fail")
        case _ => assert(false)
      }

      val z2: Either[String,Int] = Left("fail2")
      val es3 = List(a,b,z2,z)
      val t3 = Either.traverse(es3)(x => x.map(_ + 1))
      t3 match {
        case Left(x) => assert(x == "fail2")
        case _ => assert(false)
      }
    }
  }
}
