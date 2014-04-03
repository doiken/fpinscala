// package fpinscala.strictandlaziness
import scala.{Stream => _, _}
sealed abstract class Stream[+A] {
  def uncons: Option[Cons[A]]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A] = this.uncons match {
    case None => Nil
    case Some(cons) => cons.head :: cons.tail.toList
  }
}
object Empty extends Stream[Nothing] {
  val uncons = None
}
sealed abstract class Cons[+A] extends Stream[A] {
  def head: A
  def tail: Stream[A]
  val uncons = Some(this)
}
object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] {
    lazy val head = hd
    lazy val tail = tl
  }
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }
}
