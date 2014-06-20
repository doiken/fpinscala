package fpinscala.strictandlaziness

import Stream._
import scala.Option

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

  def append[B>:A](s: Stream[B]): Stream[B] = foldRight(s)((h, acc) => cons(h, acc))
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, acc) => cons(f(h), acc))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, acc) => if(f(h)) cons(h, acc) else acc)
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, acc) => f(h).append(acc))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Empty => None
    case Cons(h, t) => Some((f(h()), t()))
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)){
    case (i, Cons(h, t)) if i > 0 => Some((h(), (i - 1, t())))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B,C](a: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, a)){
    case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()), (t1(), t2())))
    case _          => None
  }

//  def zipAll2[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
//    case (Empty, Empty)             => None
//    case (Cons(h1,t1), Cons(h2,t2)) => Some(((h1(),h2()), (t1(), t2())))
//  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))
  def zipWithAll[B,C](s2: Stream[B])(f: (Option[A],Option[B]) => C): Stream[C] = {
    val a = this map (Some(_)) append (constant(None))
    val b = s2 map (Some(_)) append (constant(None))
    unfold((a, b)) {
      case (Empty, Empty) => None
      case (s1, s2) => for {
        h1 <- s1.headOption
        h2 <- s2.headOption
      } yield (f(h1,h2), (s1 drop 1, s2 drop 1))
    }
  }
  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty).forAll {
    case (h1, h2) => h1 == h2
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case s@Cons(h, t:Stream[A]) => Some((s, t))
    case _ => None
  }.append(Stream())

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)
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
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  def fibs(): Stream[Int] = {
    def go(n1:Int, n2:Int): Stream[Int] = cons(n1, go(n2, n1+n2))
    go(0, 1)
  }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None           => Stream.empty
    case Some((h, t))   => cons(h, unfold(t)(f))
  }
  def fibsViaUnfold(): Stream[Int] = unfold((0, 1)){tup =>
    Some((tup._1, (tup._2, tup._1 + tup._2)))
  }
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(a => Option(a, a + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(a => Option(a, a))

  def onesViaUnfold(): Stream[Int] = unfold(1)(a => Option(a, a))

}
