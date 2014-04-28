package fpinscala.datastructures

import org.specs2.mutable._

class ListSpec extends Specification {
  "Tree.size()" should {
    "test true" in {
      val ex1: List[Double] = Nil
      val ex2: List[Int] = Cons(1, Nil)
      val ex3: List[String] = Cons("a", Cons("b", Nil))

      true
    }
  }
  "List" should {
    "3を返す" in {
      val x = List(1,2,3,4,5) match {
        case Cons(h, Cons(2, Cons(4, _))) => h
        case Nil => 2
        case Cons(h, Cons(h2, Cons(3, Cons(4, _)))) => h + h2
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }

      x == 3
    }

    "tailで末尾を除去する" in {
      List.tail(List(1,2,3)) == List(2,3)
    }
    "setHeadで先頭を変更する" in {
      List.setHead(List(1,2,3), 3) == List(3, 2, 3)
    }
    "dropで先頭3件を破棄する" in {
      List.drop(List(1,2,3,4,5), 3) == List(4,5)
    }
    "dropWhileで先頭4未満の数値の間破棄する" in {
      List.dropWhile(List(1,2,3,4,5), {h:Int => h < 4}) == List(4,5)
    }
    "initで末尾を除いたLISTを返す" in {
      List.init(List(1,2,3,4)) == List(1,2,3)
    }
    "dropWhile2なら型指定がいらない" in {
      List.dropWhile2(List(1,2,3,4,5))({_ < 4}) == List(4,5)
    }
    // Exercise 7
    // 既に評価されているからダメなんだと...

    // Exercise 8
    "true" in {
      val v = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
      v == List(1,2,3)
    }

    "lengthはListの大きさ3を返す" in {
      val v = List.length(List(1,2,3))
      v == 3
    }
    "sum3は足し算する" in {
      val v = List.sum3(List(1,2,3))
      v == 6
    }
    "product3は掛け算する" in {
      val v = List.product3(List(1,2,3))
      v == 6
    }
    "length2は長さを返す" in {
      val v = List.length2(List(1,2,3))
      v == 3
    }
    "reverseはListを反転する" in {
      val v = List.reverse(List(1,2,3))
      v == List(3,2,1)
    }
    "incは1ずつ加算" in {
      val v = List.inc(List(1,2,3))
      v == List(2, 3, 4)
    }
    "doubleは文字列へ" in {
      val v = List.doubleToString(List(1.0,2.0,3.0))
      v == List("1.0", "2.0", "3.0")
    }
    "mapで2倍" in {
      val v = List.map(List(1,2,3))(_ * 2)
      v == List(2, 4, 6)
    }
    "filterで途中まで" in {
      val v = List.filter(List(1,2,3,2))(_ < 3)
      v == List(1, 2, 2)
    }
    "flatMapで２重に作成" in {
      val v = List.flatMap(List(1,2,3))(i => List(i,i))
      v == List(1,1,2,2,3,3)
    }
    "filterで途中までその２" in {
      val v = List.filterUsingFlatMap(List(1,2,3, 2))(_ < 3)
      v == List(1, 2, 2)
    }
    "sumEachListで合成" in {
      val v = List.sumEachList(List(1,2,3), List(2, 3))
      v == List(3, 5)
    }
    "map2で合成" in {
      val v = List.zipWith(List(1,2,3), List(2, 3))(_ + _)
      v == List(3, 5)
    }
  }
}
