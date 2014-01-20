package fpinscala.errorhandling

import org.specs2.mutable._

class OptionSpec extends Specification {
  "Option.size()" should {
    "size of one leaf is 1" in {
      Tree.size(Leaf(1)) === 1
    }
    "size of 2 leaf and 1 branch is 3" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) === 3
    }
    "size of each 2 leaf and 3 branch is 7" in {
      Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) === 7
    }
  }

  "Tree.maximum()" should {
    "1st example" in {
      Tree.maximum(Leaf(3)) === 3
    }
    "2nd example" in {
      Tree.maximum(Branch(Leaf(1), Leaf(2))) === 2
    }
    "3rd example" in {
      Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) === 4
    }
  }
}
