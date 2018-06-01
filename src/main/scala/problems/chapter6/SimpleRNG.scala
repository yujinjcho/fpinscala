package problems.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG, A]

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

  def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S) =
    rng => {
      val (x, rng2) = a(rng)
      (f(x), rng2)
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

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { i =>
      unit(f(i))
    }

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}

import State._

case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S,B]): State[S,B] =
   State(s => {
     val (a, s1) = run(s)
     f(a).run(s1)
   })

  def map[B](f: A => B): State[S,B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A,B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))
}

object State {
  def unit[S, A](a: A): State[S,A] = State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
}


sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)


object CandyMachine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(
      inputs.map(i => {
        val m: (Machine => Machine) => State[Machine, Unit] = modify[Machine] _
        val f: Input => State[Machine, Unit] = (m compose update)
        val r: State[Machine, Unit] = f(i)
        r
      })
    ).flatMap( _ =>
        get.map(s =>
          (s.coins, s.candies)))

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }
}
