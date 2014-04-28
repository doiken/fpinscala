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
//    "Return Single List" in {
//      Stream(1).toList == List(1)
//    }
//    "Return Empty List" in {
//      identity()
//      Stream().toList == List()
//    }
//    "Return Empty List" in {
//      Stream.unfold(10)(s => if (s == 0) None else Some((s, s-1))).toList must_== List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
//    }
  }
}
