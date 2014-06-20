import org.specs2.mutable._
import fpinscala.errorhandling.Option

class OptionSpec extends Specification {
  "Option" should {
    "return Some" in {
      val a = 1
      val opt = if (a > 0) Some(a) else None
      opt.map(_ * 10).getOrElse(2) mustEqual 10
      opt.flatMap(a => Some(a)).getOrElse(2) mustEqual 1
      opt.filter(a => a == 1).getOrElse(2) mustEqual 1
    }
    "return None" in {
      val a = 0
      val opt = if (a > 0) Some(a) else None
      opt.map(_ * 10).getOrElse(2) mustEqual 2
      opt.flatMap(a => Some(a)).getOrElse(2) mustEqual 2
      opt.filter(a => a == 1).getOrElse(2) mustEqual 2
    }
  }
}
