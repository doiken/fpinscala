package fpinscala.datastructures

import org.specs2.mutable._
/*
class TreeSpec extends Specification {
  "Tree.size()" should {
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

  "Tree.depth()" should {
    "1st example" in {
      Tree.depth(Leaf(3)) === 0
    }
    "2nd example" in {
      Tree.depth(Branch(Leaf(1), Leaf(2))) === 1
    }
    "3rd example" in {
      Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) === 2
    }
  }

  "Tree.map()" should{
    "map square" in {
      Tree.map(Leaf(3))((a) => (a*a)) === Leaf(9)
    }
    "map addition" in {
      Tree.map(Branch(Leaf(1), Leaf(2)))((a) => (a + 1)) === Branch(Leaf(2), Leaf(3))
    }
    "map multiplication" in {
      Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))((a) => (a * 3)) === Branch(Branch(Leaf(3), Leaf(6)), Branch(Leaf(9), Leaf(12)))
    }
  }
}
*/