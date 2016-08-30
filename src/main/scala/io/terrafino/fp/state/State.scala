package io.terrafino.fp.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }

  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (i1, rng2) = ra(rng)
      val (i2, rng3) = rb(rng2)
      (f(i1, i2), rng3)
    }

  def flatMap[A, B](s: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (i, rng2) = s(rng)
      g(i)(rng2)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(i => unit(f(i)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    val res = Math.abs(if (n == Int.MinValue) n + 1 else n)
    (res, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = RNG.nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), rng2)
  }
  def double2(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = RNG.intDouble(rng)
    ((d, i), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = RNG.double(rng)
    val (d2, rng3) = RNG.double(rng2)
    val (d3, rng4) = RNG.double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(rng: RNG, n: Int, acc: List[Int]): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        val (i, rng2) = rng.nextInt
        go(rng2, n - 1, acc :+ i)
      }
    }
    go(rng, count, List())
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S, +A](run: S => (A, S)) {

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s2) = run(s)
    (f(a), s2)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s)
    (f(a, b), s3)
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { s =>
    val start = ((s.candies, s.coins), s)
    inputs.foldRight(start)((i, p) => {
      val candies = p._1._1
      val coins = p._1._2
      val m = p._2
      i match {
        case Coin if m.locked && candies > 0 => ((candies, coins+1), Machine(false, candies, coins+1))
        case Turn if !m.locked => ((candies-1, coins), Machine(true, candies-1, coins))
        case Turn if m.locked => ((candies, coins), Machine(true, candies, coins))
        case Coin if !m.locked => ((candies, coins), Machine(false, candies, coins))
        case _ => ((candies, coins), Machine(false, candies, coins))
      }
    })
  }
}
