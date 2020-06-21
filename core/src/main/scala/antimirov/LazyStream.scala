package antimirov

sealed abstract class LazyStream[+A] { self =>

  import LazyStream.{Empty, Defer, Cons}

  def force: LazyStream.Eager[A] =
    this match {
      case e: LazyStream.Eager[A] => e
      case d: Defer[A] => d.under.force
    }

  def isEmpty: Boolean =
    force match {
      case Cons(_, _) => false
      case Empty => true
    }

  def nonEmpty: Boolean =
    force match {
      case Cons(_, _) => true
      case Empty => false
    }

  def map[B](f: A => B): LazyStream[B] =
    this match {
      case Cons(a, t) => Cons(f(a), t.map(f))
      case Empty => Empty
      case d: Defer[A] => LazyStream.defer(d.force.map(f))
    }

  def flatMap[B](f: A => LazyStream[B]): LazyStream[B] =
    this match {
      case Cons(a, t) => LazyStream.concat(f(a), t.flatMap(f))
      case Empty => Empty
      case d: Defer[A] => LazyStream.defer(d.force.flatMap(f))
    }

  def toList: List[A] =
    iterator.toList

  def iterator: Iterator[A] =
    new Iterator[A] {
      var xs: LazyStream[A] = self
      def hasNext: Boolean =
        xs.nonEmpty
      def next(): A =
        xs.force match {
          case Cons(h, t) =>
            xs = t
            h
          case Empty =>
            throw new NoSuchElementException("next on empty iterator")
        }
    }
}


object LazyStream {

  sealed abstract class Eager[+A] extends LazyStream[A]

  def empty[A]: LazyStream[A] = Empty
  def apply[A](a: A): LazyStream[A] = Cons(a, Empty)
  def defer[A](thunk: => LazyStream[A]): LazyStream[A] = new Defer(thunk)

  def concat[A](lhs: LazyStream[A], rhs: => LazyStream[A]): LazyStream[A] =
    defer(lhs.force match {
      case Cons(hx, tx) => Cons(hx, concat(tx, rhs))
      case Empty => rhs
    })

  def fromIterable[A](as: Iterable[A]): LazyStream[A] =
    fromIterator(as.iterator)

  def fromIterator[A](it: Iterator[A]): LazyStream[A] =
    new Defer(if (it.hasNext) Cons(it.next, fromIterator(it)) else Empty)

  case class Cons[A](head: A, tail: LazyStream[A]) extends Eager[A]
  case object Empty extends Eager[Nothing]
  class Defer[A](thunk: => LazyStream[A]) extends LazyStream[A] {
    lazy val under: LazyStream[A] = thunk
  }
}
