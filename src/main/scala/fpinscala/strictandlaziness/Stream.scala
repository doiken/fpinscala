package fpinscala.strictandlaziness

import Stream._
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
  def take2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => Stream()
  }
  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) => cons(h(), t().take(n-1))
      case _ => Stream.empty // we can say Stream.empty
    }
    else Stream()            // or Stream()

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t)          => cons(h(), t().drop(n - 1))
    case Empty => Stream.empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]){ (h, acc) =>
      if (p(h)) cons(h, acc)
      else empty[A]
    }
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A]){ (h, _) => Some(h)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
