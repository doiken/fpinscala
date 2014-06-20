package fpinscala.state

import org.specs2.mutable._
import fpinscala.state.RNG.Simple

class RNGSpec extends Specification {
  "Simple" should {
    "nextInt return purely" in {
      val rng = Simple(42)
      val (n1, rng2) = rng.nextInt
      n1 mustEqual 16159453

      val (n2, rng3) = rng2.nextInt
      n2 mustEqual -1281479697
    }
    "nonNegativeInt return nonNegative" in {
      val rng = Simple(42)
      val (_, rng2) = rng.nextInt
      val (n2, _) = RNG.nonNegativeInt(rng2)
      n2 mustEqual 1281479697
    }
    "double return double" in {
      val rng = Simple(42)
      val (_, rng2) = rng.nextInt
      val (n2, _) = RNG.double(rng2)
      n2 < 1 mustEqual true
    }
    "ints" in {
      val rng = Simple(42)
      val (list, rng2) = RNG.ints(2)(rng)
      list.head mustEqual 16159453
      list.tail.head mustEqual -1281479697
    }
    "double2 return double value" in {
      val rng = Simple(42)
      val (_, rng2) = rng.nextInt
      val (n2, rng3) = RNG.double2(rng2)
      n2 < 1 mustEqual true
    }
    "test randIntDouble using map2" in {
      val rng = Simple(42)
      val ((n1, n2), _) = RNG.map2(RNG.int, RNG.double)((_, _))(rng)
      n1 mustEqual 16159453
      n2 mustEqual 0.5967354856416283d
    }
/*    "flatMap" in {
      val rng = Simple(42)
      val (i, rng) = RNG.flatMap(RNG.double)(b => ())(rng)
      RNG.flatMap(RNG.nonNegativeInt)(i => if (i + (n-1) - mod >= 0))(rng)
    }*/
    "nonNegativeInt works Cool" in {
      val rng = Simple(42)
      val (i, _) = RNG.nonNegativeLessThan(2)(rng)

      i < 2 mustEqual true
      i > 0 mustEqual true
      // property based test shitai
    }
    "test mapViaFlatMap" in {
      val rng = Simple(42)
      val (n1, _) = RNG.mapViaFlatMap(RNG.int)(_ * 2)(rng)

      n1 mustEqual 16159453 * 2
    }
    "test map2ViaFlatMap" in {
      val rng = Simple(42)
      val ((n1, n2), _) = RNG.map2ViaFlatMap(RNG.int, RNG.double)((_, _))(rng)
      n1 mustEqual 16159453
      n2 mustEqual 0.5967354856416283d
    }
  }
}
