package fpinscala.strictandlaziness

import org.specs2.mutable._

class StreamSpec extends Specification {
  "Stream.toList()" should {
    "Return Multi List" in {
      Stream(1, 2, 3).toList == List(1,2,3)
    }
    "Return Take" in {
      Stream(1, 2, 3).take(2).toList mustEqual List(1, 2)
    }
    "Return Drop" in {
      Stream(1, 2, 3).drop(2).toList mustEqual List(3)
    }
    "Return TakeWhile" in {
      Stream(1, 2, 3).takeWhile(a => a < 3).toList mustEqual List(1, 2)
    }
    "Return forAll" in {
      Stream(1, 2, 3).forAll(a => a < 3) mustEqual false
    }
    "Return Append" in {
      Stream(1, 2, 3).append(Stream(4)).toList mustEqual List(1, 2, 3, 4)
    }
    "Return Append" in {
      Stream(1, 2, 3).append(Stream(4)).toList mustEqual List(1, 2, 3, 4)
    }
    "Return Map" in {
      Stream(1, 2, 3).map(_ * 2).toList mustEqual List(2, 4, 6)
    }
    "Return Filter" in {
      Stream(1, 2, 3).filter(_ < 3).toList mustEqual List(1, 2)
    }
    "Return flatMap" in {
      Stream(1, 2, 3).flatMap(h => Stream(h, h)).toList mustEqual List(1, 1, 2, 2, 3, 3)
    }
    "Return Constant" in {
      Stream.constant(1).take(3).toList mustEqual List(1, 1, 1)
    }
    "Return from" in {
      Stream.from(1).take(3).toList mustEqual List(1, 2, 3)
    }
    "Return fibs" in {
      Stream.fibs().take(1).toList mustEqual List(0)
      Stream.fibs().take(4).toList mustEqual List(0, 1, 1, 2)
    }
    "Return unfold" in {
      Stream.unfold(10)(i => if (i == 0) None else Some(i, i - 1)).take(3).toList mustEqual List(10, 9, 8)
    }
    "Return fibsViaUnfold" in {
      Stream.fibsViaUnfold().take(4).toList mustEqual List(0, 1, 1, 2)
    }
    "Return from" in {
      Stream.fromViaUnfold(1).take(3).toList mustEqual List(1, 2, 3)
    }
    "Return Map" in {
      Stream(1, 2, 3).mapViaUnfold(_ * 2).toList mustEqual List(2, 4, 6)
    }
    "Return Take" in {
      Stream(1, 2, 3).takeViaUnfold(2).toList mustEqual List(1, 2)
    }
    "Return TakeWhile" in {
      Stream(1, 2, 3).takeWhileViaUnfold(a => a < 3).toList mustEqual List(1, 2)
    }
    "Return StartWith" in {
      Stream(1,2,3).startsWith(Stream(1,2)) mustEqual true
      Stream(1,2,3).startsWith(Stream(2)) mustEqual false
    }
//    "Return Empty List" in {
//      identity()
//      Stream().toList == List()
//    }
//    "Return Empty List" in {
//      Stream.unfold(10)(s => if (s == 0) None else Some((s, s-1))).toList must_== List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
//    }
  }
}
