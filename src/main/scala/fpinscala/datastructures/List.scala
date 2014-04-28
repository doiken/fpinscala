package fpinscala.datastructures
sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: List[A]): List[A] = ds match {
    case Cons(h, t) => t
    case _ => Nil
  }
  def setHead[A](ds: List[A], newHead: A): List[A] = ds match {
    case Cons(h, t) => Cons(newHead, t)
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(h, t) if n <= 1 => t
    case Cons(h, t) => drop(t, n -1)
    case _ => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(h, t) => Cons(h, t)
    case _ => Nil
  }

  /* 引数の分離により、型を明示しなくて良い */
  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile2(t)(f)
      case _ => l }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  def init[A](l: List[A]): List[A] = l match {
    case Cons(h,Nil) => Nil
    case Cons(h,t) => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B)(f: (B, A) => B): B = l match {
    case Nil => acc
    case Cons(h,t) => foldLeft(t, f(acc,h))(f)
  }

  def sum3(ints: List[Int]): Int =  foldLeft(ints, 0)(_ + _)

  def product3(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc,_) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRightViaLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(List.reverse(l), z)((b, a) => f(a, b))

//  def foldLeftViaRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((h, t) => Cons(h,t))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())((a, b) => append(a, b))

  def inc[A](l: List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString[A](l: List[Double]): List[String] = foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil:List[A])((a, b) => if(f(a)) Cons(a, b) else b)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldLeft(l, List[B]())((acc, h) => List.append(acc, f(h)))

  def filterUsingFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

  def sumEachList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, sumEachList(t1, t2))
  }
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => false
    case Cons(h,t) if startsWith(l, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }
}

