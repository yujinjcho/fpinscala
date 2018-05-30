package problems.chapter6


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  val int: Rand[Int] = _.nextInt

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

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => (i / (Int.MaxValue.toDouble + 1)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDoubleViaBoth: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleIntViaBoth: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f, acc)(_ :: _))

  def intsViaSequence(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))
  }
}
