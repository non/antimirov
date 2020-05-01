package antimirov

import java.lang.Double.isNaN
import scala.collection.immutable.Queue

sealed abstract class Re { lhs =>

  import Re._

  lazy val firstSet: Set[Char] =
    this match {
      case Phi => Set.empty
      case Empty => Set.empty
      case Letter(c) => Set(c)
      case Choice(r1, r2) => r1.firstSet | r2.firstSet
      case Cat(r1, r2) if r1.matchesEmpty => r1.firstSet | r2.firstSet
      case Cat(r1, r2) => r1.firstSet
      case Star(r) => r.firstSet
      case Var(_) => sys.error("!")
    }

  def accepts1(s: String): Boolean = {
    def recur(r: Re, i: Int): Boolean =
      if (i >= s.length) {
        r.matchesEmpty
      } else {
        val c = s.charAt(i)
        if (!r.firstSet(c)) false
        else recur(r.deriv(c), i + 1)
      }
    recur(this, 0)
  }

  def accepts(s: String): Boolean = {

    def interleave[A](xs: Stream[A], ys: Stream[A]): Stream[A] =
      if (xs.isEmpty) ys else xs.head #:: interleave(ys, xs.tail)

    def has(c: Char, i: Int): Boolean =
      i < s.length && s.charAt(i) == c

    def look(r: Re, pos: Int): Stream[Int] =
      if (pos > s.length) Stream.empty
      else r match {
        case Phi => Stream.empty
        case Empty => Stream(pos)
        case Letter(c) if has(c, pos) => Stream(pos + 1)
        case Letter(_) => Stream.empty
        case Choice(r1, r2) =>
          val o = if (r1.matchesEmpty || r2.matchesEmpty) Stream(pos) else Stream.empty
          consume(r1, pos) #::: consume(r2, pos) #::: o
        case Cat(r1, r2) =>
          val o = if (r1.matchesEmpty) {
            val s = consume(r2, pos)
            if (r2.matchesEmpty) s #::: Stream(pos) else s
          } else {
            Stream.empty
          }
          consume(r1, pos).flatMap(look(r2, _)) #::: o
        case Star(x) => consume(x * r, pos) #::: Stream(pos)
        case Var(_) => sys.error("!")
      }

    def consume(r: Re, pos: Int): Stream[Int] =
      if (pos > s.length) Stream.empty
      else r match {
        case Phi | Empty => Stream.empty
        case Letter(c) if has(c, pos) => Stream(pos + 1)
        case Letter(_) => Stream.empty
        case Choice(r1, r2) => consume(r1, pos) #::: consume(r2, pos)
        case Cat(r1, r2) => consume(r1, pos).flatMap(look(r2, _))
        case Star(x) => consume(x * r, pos)
        case Var(_) => sys.error("!")
      }

    look(this, 0).exists(_ == s.length)
  }

  def +(rhs: Re): Re =
    if (lhs == Phi) rhs
    else if (rhs == Phi) lhs
    else if (lhs == rhs) lhs //FIXME
    else Choice(lhs, rhs)

  def |(rhs: Re): Re =
    lhs + rhs

  def *:(prefix: Char): Re =
    Re(prefix) * this

  def :*(suffix: Char): Re =
    this * Re(suffix)

  def *(rhs: Re): Re =
    if (lhs == Phi || rhs == Phi) Phi
    else if (lhs == Empty) rhs
    else if (rhs == Empty) lhs
    else Cat(lhs, rhs)

  def star: Re =
    this match {
      case Phi | Empty => Empty
      case Star(_) => this
      case _ => Star(this)
    }

  def &(rhs: Re): Re =
    Re.intersect(lhs, rhs)

  def -(rhs: Re): Re =
    Re.difference(lhs, rhs)

  def ^(rhs: Re): Re =
    Re.xor(lhs, rhs)

  def equiv(rhs: Re): Boolean = {
    def recur(env: Set[(Re, Re)], pair: (Re, Re)): Boolean =
      pair match {
        case (r1, r2) if r1.matchesEmpty != r2.matchesEmpty => false
        case (r1, r2) if r1.isPhi != r2.isPhi => false
        case _ if env(pair) => true
        case (r1, r2) =>
          val letters = r1.alphabet
          if (letters == r2.alphabet) {
            val env2 = env + pair
            letters.forall(c => recur(env2, (r1.deriv(c), r2.deriv(c))))
          } else {
            false
          }
      }
    recur(Set.empty, (lhs, rhs))
  }

  def ===(rhs: Re): Boolean =
    lhs equiv rhs

  def <(rhs: Re): Boolean =
    partialCompare(rhs) < 0.0

  def >(rhs: Re): Boolean =
    partialCompare(rhs) > 0.0

  def <=(rhs: Re): Boolean =
    partialCompare(rhs) <= 0.0

  def >=(rhs: Re): Boolean =
    partialCompare(rhs) >= 0.0

  def subsetOf(rhs: Re): Boolean =
    partialCompare(rhs) <= 0.0

  def supersetOf(rhs: Re): Boolean =
    partialCompare(rhs) >= 0.0

  def properSubsetOf(rhs: Re): Boolean =
    partialCompare(rhs) < 0.0

  def properSupersetOf(rhs: Re): Boolean =
    partialCompare(rhs) > 0.0

  override def toString: String =
    this match {
      case Phi => "ϕ"
      case Empty => "ε"
      case Letter(c) => s"$c"
      case Choice(r1, r2) => s"($r1|$r2)"
      case Cat(r1, r2) => s"$r1$r2"
      case Star(Letter(c)) => s"$c*"
      case Star(r) => s"($r)*"
      case Var(x) => "$" + x.toString
    }

  def isPhi: Boolean =
    this match {
      case Phi => true
      case Empty | Letter(_) | Star(_) | Var(_) => false
      case Choice(r1, r2) => r1.isPhi && r2.isPhi
      case Cat(r1, r2) => r1.isPhi || r2.isPhi
    }

  def isEmpty: Boolean =
    this match {
      case Empty => true
      case Phi | Letter(_) | Star(_) | Var(_) => false
      case Choice(r1, r2) => r1.isEmpty && r2.isEmpty
      case Cat(r1, r2) => r1.isEmpty || r2.isEmpty
    }

  def simplified: Re =
    this match {
      case Empty | Phi | Letter(_) =>
        this
      case Cat(x, y) =>
        x.simplified * y.simplified
      case Choice(x, y) =>
        val (x2, y2) = (x.simplified, y.simplified)
        if (x2 == y2) x2 else x2 + y2
      case Star(r) =>
        r.simplified.star
      case Var(_) =>
        sys.error("!")
    }

  def matchesEmpty: Boolean =
    this match {
      case Empty | Star(_) => true
      case Phi | Letter(_) => false
      case Choice(r1, r2) => r1.matchesEmpty || r2.matchesEmpty
      case Cat(r1, r2) => r1.matchesEmpty && r2.matchesEmpty
      case Var(_) => sys.error("!")
    }

  lazy val alphabet: Set[Char] =
    this match {
      case Letter(c) => Set(c)
      case Choice(r1, r2) => r1.alphabet | r2.alphabet
      case Cat(r1, r2) => r1.alphabet | r2.alphabet
      case Star(r) => r.alphabet
      case Empty | Phi | Var(_) => Set.empty
    }

  def deriv(c: Char): Re =
    Re.choice(partDeriv(c))

  def partDeriv(x: Char): Set[Re] =
    this match {
      case Phi | Empty => Set.empty
      case Letter(c) if c == x => Set(Empty)
      case Letter(_) => Set.empty
      case Choice(r1, r2) => r1.partDeriv(x) | r2.partDeriv(x)
      case Star(r) => r.partDeriv(x).filter(_ != Phi).map(_ * this)
      case Cat(r1, r2) =>
        val s1 = r1.partDeriv(x).map(_ * r2)
        if (r1.matchesEmpty) s1 | r2.partDeriv(x) else s1
    }

  def mentions(x: Int): Boolean =
    this match {
      case Var(y) => x == y
      case Cat(r1, r2) => r1.mentions(x) || r2.mentions(x)
      case Choice(r1, r2) => r1.mentions(x) || r2.mentions(x)
      case Star(r) => r.mentions(x)
      case _ => false
    }

  def resolve(x: Int): Re = {
    def recur(r: Re, x: Int): (List[Re], List[Re]) =
      r match {
        case v @ Var(y) =>
          if (y == x) (List(Empty), Nil) else (Nil, List(v))
        case Cat(r1, r2) =>
          val (rs1, bs1) = recur(r1, x)
          val (rs2, bs2) = recur(r2, x)
          (cart(rs1, rs2) ::: cart(rs1, bs2) ::: cart(bs1, rs2), cart(bs1, bs2))
        case Choice(r1, r2) =>
          val (rs1, bs1) = recur(r1, x)
          val (rs2, bs2) = recur(r2, x)
          (rs1 ::: rs2, bs1 ::: bs2)
        case r =>
          (Nil, List(r))
      }
    val (rs, bs) = recur(this, x)
    Re.choice(rs).star * Re.choice(bs)
  }

  // -1 means (lhs < rhs) means (lhs subsetOf rhs)
  //  0 means (lhs = rhs) means (lhs equiv rhs)
  // +1 means (lhs > rhs) means (lhs supsersetOf rhs)
  // NaN means none of the above
  def partialCompare(rhs: Re): Double = {

    def recur(env: Set[(Re, Re)], pair: (Re, Re)): Double =
      pair match {
        case (Phi, rhs) =>
          if (rhs.isPhi) 0.0 else -1.0
        case (lhs, Phi) =>
          if (lhs.isPhi) 0.0 else 1.0
        case (Empty, rhs) =>
          if (rhs.isEmpty) 0.0
          else if (rhs.matchesEmpty) -1.0
          else Double.NaN
        case (lhs, Empty) =>
          if (lhs.isEmpty) 0.0
          else if (lhs.matchesEmpty) 1.0
          else Double.NaN
        case _ if env(pair) =>
          0.0
        case (lhs, rhs) =>

          var res = setCompare(lhs.alphabet, rhs.alphabet)
          if (isNaN(res)) return res

          (lhs.matchesEmpty, rhs.matchesEmpty) match {
            case (false, true) => res = acc(res, -1.0)
            case (true, false) => res = acc(res, 1.0)
            case _ => ()
          }
          if (isNaN(res)) return res

          val letters = if (res <= 0.0) lhs.alphabet else rhs.alphabet
          val env2 = env + pair

          val it = letters.iterator
          while (it.hasNext && !isNaN(res)) {
            val c = it.next
            val x = recur(env2, (lhs.deriv(c), rhs.deriv(c)))
            res = acc(res, x)
          }
          res
      }

    if (lhs == rhs) 0.0
    else recur(Set.empty, (lhs, rhs))
  }
}

object Re {

  def zero: Re = Phi
  def lambda: Re = Empty
  def apply(c: Char): Re = Letter(c)
  def choice(rs: Iterable[Re]): Re =
    if (rs.isEmpty) Phi else rs.reduceLeft(_ + _)

  def universe(alphabet: Set[Char]): Re =
    Re.choice(alphabet.map(Re(_))).star

  case object Phi extends Re // matches nothing
  case object Empty extends Re // matches empty string ("")
  case class Letter(c: Char) extends Re // single character
  case class Choice(r1: Re, r2: Re) extends Re // either
  case class Cat(r1: Re, r2: Re) extends Re // concatenation
  case class Star(r: Re) extends Re
  case class Var(x: Int) extends Re

  def intersect(r1: Re, r2: Re): Re = {
    def recur(cnt: Int, env: Map[(Re, Re), Re], pair: (Re, Re)): Re = {
      val (r1, r2) = pair
      pair match {
        case (Phi, _) | (_, Phi) => Phi
        case (Empty, r2) => if (r2.matchesEmpty) Empty else Phi
        case (r1, Empty) => if (r1.matchesEmpty) Empty else Phi
        case (r1, r2) =>
          env.get(pair) match {
            case Some(res) =>
              res
            case None =>
              val letters = r1.firstSet & r2.firstSet
              val env2 = env.updated(pair, Var(cnt))
              def f(c: Char): Re = Re(c) * recur(cnt + 1, env2, (r1.deriv(c), r2.deriv(c)))
              val rr = Re.choice(letters.map(f))
              val rr2 = if (r1.matchesEmpty && r2.matchesEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  def difference(r1: Re, r2: Re): Re = {
    def recur(cnt: Int, env: Map[(Re, Re), Re], pair: (Re, Re)): Re = {
      val (r1, r2) = pair
      pair match {
        case (Phi, _) => Phi
        case (Empty, r2) => if (r2.matchesEmpty) Phi else Empty
        case (_, Phi) => r1
        case (r1, r2) =>
          env.get(pair) match {
            case Some(res) =>
              res
            case None =>
              val letters = r1.firstSet
              val env2 = env.updated(pair, Var(cnt))
              def f(c: Char): Re = Re(c) * recur(cnt + 1, env2, (r1.deriv(c), r2.deriv(c)))
              val rr = Re.choice(letters.map(f))
              val rr2 = if (r1.matchesEmpty && !r2.matchesEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  def xor(r1: Re, r2: Re): Re = {
    def recur(cnt: Int, env: Map[(Re, Re), Re], pair: (Re, Re)): Re = {
      val (r1, r2) = pair
      pair match {
        case (r1, Phi) => r1
        case (Phi, r2) => r2
        case (Empty, r2) if !r2.matchesEmpty => r2 + Empty
        case (r1, Empty) if !r1.matchesEmpty => r1 + Empty
        case (r1, r2) =>
          env.get(pair) match {
            case Some(res) =>
              res
            case None =>
              val letters = r1.firstSet | r2.firstSet
              val env2 = env.updated(pair, Var(cnt))
              def f(c: Char): Re = Re(c) * recur(cnt + 1, env2, (r1.deriv(c), r2.deriv(c)))
              val rr = Re.choice(letters.map(f))
              val rr2 = if (r1.matchesEmpty ^ r2.matchesEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  // cartesian product
  def cart(xs: List[Re], ys: List[Re]): List[Re] =
    for { x <- xs; y <- ys } yield x * y

  // operation table
  //    -1  0 +1 Na
  // -1 -1 -1 Na Na
  //  0 -1  0 +1 Na
  // +1 Na +1 +1 Na
  // Na Na Na Na Na
  def acc(x: Double, y: Double): Double =
    if (x == 0.0 || Math.signum(x) == Math.signum(y)) y
    else if (y == 0.0) x
    else Double.NaN

  def setCompare(xs: Set[Char], ys: Set[Char]): Double =
    if (xs.size > ys.size) {
      -setCompare(ys, xs)
    } else if (xs.size == ys.size) {
      if (xs == ys) 0.0 else Double.NaN
    } else {
      if (xs.forall(ys(_))) -1.0 else Double.NaN
    }
}
