package antimirov

sealed abstract class LazyStream[+A] { self =>

  import LazyStream.{Empty, Defer, Cons}

  def force: LazyStream.Eager[A] =
    this match {
      case e: LazyStream.Eager[A] => e
      case d: Defer[A] => d.under.force
    }

  def isEmpty: Boolean =
    !nonEmpty

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

  def merge[A](xs: LazyStream[A], ys: LazyStream[A])(lt: (A, A) => Boolean): LazyStream[A] =
    defer((xs.force, ys.force) match {
      case (Cons(hx, tx), Cons(hy, ty)) =>
        if (lt(hx, hy)) Cons(hx, merge(tx, ys)(lt)) else Cons(hy, merge(xs, ty)(lt))
      case (Empty, eys) =>
        eys
      case (exs, Empty) =>
        exs
    })

  def mergeAll[A](xss: Vector[LazyStream[A]])(lt: (A, A) => Boolean): LazyStream[A] =
    xss.size match {
      case n if n < 1 =>
        LazyStream.empty[A]
      case 1 =>
        xss(0)
      case 2 =>
        merge(xss(0), xss(1))(lt)
      case n =>
        val ln = n / 2
        merge(mergeAll(xss.slice(0, ln))(lt), mergeAll(xss.slice(ln, n))(lt))(lt)
    }

  def fromIterator[A](it: Iterator[A]): LazyStream[A] =
    new Defer(if (it.hasNext) Cons(it.next, fromIterator(it)) else Empty)

  case class Cons[A](head: A, tail: LazyStream[A]) extends Eager[A]
  case object Empty extends Eager[Nothing]
  class Defer[A](thunk: => LazyStream[A]) extends LazyStream[A] {
    lazy val under: LazyStream[A] = thunk
  }
}
