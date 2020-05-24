package antimirov

sealed abstract class LazyStream[+A] { self =>

  import LazyStream.{Empty, Defer, Cons}

  def nonEmpty: Boolean =
    this match {
      case Empty => false
      case Cons(_, _) => true
      case d: Defer[A] => d.under.nonEmpty
    }

  def iterator: Iterator[A] =
    new Iterator[A] {
      var xs: LazyStream[A] = self
      def hasNext: Boolean = xs.nonEmpty
      def next(): A =
        xs match {
          case Cons(h, t) =>
            xs = t
            h
          case d: Defer[A] =>
            xs = d.under
            next
          case Empty =>
            sys.error("!")
        }
    }
}

object LazyStream {

  def empty[A]: LazyStream[A] = Empty

  def apply[A](a: A): LazyStream[A] = Cons(a, Empty)

  def fromIterator[A](it: Iterator[A]): LazyStream[A] =
    new Defer(if (it.hasNext) Cons(it.next, fromIterator(it)) else Empty)

  case class Cons[A](head: A, tail: LazyStream[A]) extends LazyStream[A]
  case object Empty extends LazyStream[Nothing]

  class Defer[A](thunk: => LazyStream[A]) extends LazyStream[A] {
    lazy val under: LazyStream[A] = thunk
  }
}
