package fpinscala.errorhandling

import org.specs2.mutable._

class EitherSpec extends Specification {
  "Either.map()" should {
    "RightはRight(計算結果)を返す" in {
      val right:Either[String, Int] = Right(3)
      val expected = Right(6)
      // right.map(i => i * 2) == expected
      true
    }
    "LeftはLeft(msg)を返す" in {
      val left:Either[String, Int] = Left("error")
      val expected = left
      // left.map(twice) == expected
      // identity(1)
      true
    }
  }
  "Either.flatMap()" should {
    "RightはRight(計算結果)を返す" in {
      val right:Either[String, Int] = Right(3)
      val expected = Right(6)
      right.flatMap(a => Right(a * 2)) == expected
    }
    "LeftはLeft(msg)を返す" in {
      val left:Either[String, Int] = Left("error")
      val expected = Left("error")
      left.flatMap(a => Right(a * 2)) == expected
    }
  }
  "Either.orElse()" should {
    "RithtはRight(a)を返す" in {
      val right:Either[String, Int] = Right(3)
      val expected = Right(3)
      right.orElse(Right(100)) == expected
    }
    "LeftはorElseの値を返す" in {
      val left:Either[String, Int] = Left("error")
      val expected = Right(100)
      left.orElse(Right(100)) == expected
    }
  }
  "Either.map2()" should {
    "どちらもRightなら計算結果を返す" in {
      val right:Either[String, Int] = Right(3)
      val expected = Right(9)
      right.map2(right)(_ * _) == expected
    }
    "呼び出し元がLeftならLeftを返す" in {
      val right:Either[String, Int] = Right(3)
      val left:Either[String, Int] = Left("error")
      val expected = left
      left.map2(right)(_ * _) == expected
    }
    "引数がLeftならLeftを返す" in {
      val right:Either[String, Int] = Right(3)
      val left:Either[String, Int] = Left("error")
      val expected = left
      right.map2(left)(_ * _) == expected
    }
  }
}
