import org.specs2.mutable._
import fpinscala.errorhandling.Option


class OptionSpec extends Specification {
  "Option.size()" should {
    "return true" in {
      val opt = None
      print(opt.getOrElse(1))
      true
    }
  }
}
