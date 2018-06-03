package problems.chapter6

import org.scalatest._

class Chapter6Spec extends FeatureSpec {
  feature("CandyMachine") {
    scenario("test") {
      val inputs: List[Input] = List(Turn, Coin, Turn)
      val r: State[Machine, (Int,Int)] = CandyMachine.simulateMachine(inputs)

      assert(r.run(Machine(false, 10, 10))._2 == Machine(true, 8, 11))
    }
  }
}
