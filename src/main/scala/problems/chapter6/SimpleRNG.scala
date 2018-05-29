package problems.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    (if (n < 0) -(n + 1) else n, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (randInt,rng1) = rng.nextInt
    val (randDouble,rng2) = double(rng1)
    ((randInt,randDouble),rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (randDouble,rng1) = double(rng)
    val (randInt,rng2) = rng1.nextInt
    ((randDouble,randInt),rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (randDouble1,rng1) = double(rng)
    val (randDouble2,rng2) = double(rng1)
    val (randDouble3,rng3) = double(rng2)
    ((randDouble1, randDouble2, randDouble3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (List(), rng)
    } else {
      val (i, nextRng) = rng.nextInt
      val (l, rngRecursive) = ints(count - 1)(nextRng)
      (l :+ i, nextRng)
    }
  }
}
