package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  def int(rng: RNG): (Int, RNG) = {
    rng.nextInt
  }
  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def doublePair(rng: RNG): ((Double, Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    ((d1,d2), rng3)
  }
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (tmpI, nextRNG) = rng.nextInt
    val i = if (tmpI == Int.MinValue) 0 else tmpI.abs
    (i, nextRNG)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (tmpI, nextRNG) = nonNegativeInt(rng)
    (tmpI / Int.MaxValue.toDouble, nextRNG)
  }
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    ((i, d), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  type Rand[+A] = RNG => (A, RNG)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)
  def double2: Rand[Double] = {
    map(nonNegativeInt)(i => i / Int.MaxValue.toDouble)
  }
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }
  }
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap{
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      ((a, b), rng3)
    }
  }(t => unit(f(t._1:A, t._2:B)))
  def map2ViaFlatMap2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

//  def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
}

