package antimirov

import java.lang.Double.isNaN
import scala.collection.immutable.Queue

sealed abstract class Rx { lhs =>

  import Rx._

  lazy val firstRanges: Stream[(Char, Char)] = {
    def recur(r: Rx): Iterator[(Char, Char)] =
      r match {
        case Phi => Iterator.empty
        case Empty => Iterator.empty
        case Letter(c) => Iterator((c, c))
        case Letters(cs) => cs.ranges
        case Choice(r1, r2) =>
          LetterSet.diff(recur(r1), recur(r2)).map(_.value)
        case Cat(r1, r2) if r1.matchesEmpty =>
          LetterSet.diff(recur(r1), recur(r2)).map(_.value)
        case Cat(r1, _) => recur(r1)
        case Star(r) => recur(r)
        case Var(_) => sys.error("!")
      }
    recur(this).toStream
  }

  def rejects(s: String): Boolean =
    !accepts(s)

  def accepts(s: String): Boolean = {
    def recur(r: Rx, i: Int): Stream[Unit] =
      if (i >= s.length) {
        if (r.matchesEmpty) Stream(()) else Stream.empty
      } else {
        r.partialDeriv(s.charAt(i)).toStream.flatMap(recur(_, i + 1))
      }
    recur(this, 0).nonEmpty
  }

  def +(rhs: Rx): Rx =
    (lhs, rhs) match {
      case (x, y) if x == y => x
      case (Phi, _) => rhs
      case (_, Phi) => lhs
      case (Letter(c1), Letter(c2)) => Letters(LetterSet(c1, c2))
      case (Letters(cs1), Letter(c2)) => Letters(cs1 + c2)
      case (Letter(c1), Letters(cs2)) => Letters(cs2 + c1)
      case (Letters(cs1), Letters(cs2)) => Letters(cs1 | cs2)
      case _ => Choice(lhs, rhs)
    }

  def |(rhs: Rx): Rx =
    lhs + rhs

  def *:(prefix: Char): Rx =
    Rx(prefix) * this

  def :*(suffix: Char): Rx =
    this * Rx(suffix)

  def *(rhs: Rx): Rx =
    if (lhs == Phi || rhs == Phi) Phi
    else if (lhs == Empty) rhs
    else if (rhs == Empty) lhs
    else Cat(lhs, rhs)

  def star: Rx =
    this match {
      case Phi | Empty => Empty
      case Star(_) => this
      case _ => Star(this)
    }

  def &(rhs: Rx): Rx =
    Rx.intersect(lhs, rhs)

  def -(rhs: Rx): Rx =
    Rx.difference(lhs, rhs)

  def ^(rhs: Rx): Rx =
    Rx.xor(lhs, rhs)

  def equiv(rhs: Rx): Boolean = {
    def recur(env: Set[(Rx, Rx)], pair: (Rx, Rx)): Boolean =
      pair match {
        case (r1, r2) if r1.matchesEmpty != r2.matchesEmpty => false
        case (r1, r2) if r1.isPhi != r2.isPhi => false
        case _ if env(pair) => true
        case (r1, r2) =>
          val (ranges1, ranges2) = (r1.firstRanges, r2.firstRanges)

          val alpha: Stream[(Boolean, Char)] =
            LetterSet.diff(ranges1.iterator, ranges2.iterator).map {
              case Diff.Both((c, _)) => (true, c)
              case _ => (false, '\u0000')
            }.toStream
          val env2 = env + pair
          alpha.forall { case (ok, c) =>
            ok && recur(env2, (r1.deriv(c), r2.deriv(c)))
          }
      }
    recur(Set.empty, (lhs, rhs))
  }

  def ===(rhs: Rx): Boolean =
    lhs equiv rhs

  def <(rhs: Rx): Boolean =
    partialCompare(rhs) < 0.0

  def >(rhs: Rx): Boolean =
    partialCompare(rhs) > 0.0

  def <=(rhs: Rx): Boolean =
    partialCompare(rhs) <= 0.0

  def >=(rhs: Rx): Boolean =
    partialCompare(rhs) >= 0.0

  def subsetOf(rhs: Rx): Boolean =
    partialCompare(rhs) <= 0.0

  def supersetOf(rhs: Rx): Boolean =
    partialCompare(rhs) >= 0.0

  def properSubsetOf(rhs: Rx): Boolean =
    partialCompare(rhs) < 0.0

  def properSupersetOf(rhs: Rx): Boolean =
    partialCompare(rhs) > 0.0

  def repr: String = {
    def choices(re: Rx): List[Rx] =
      re match {
        case Choice(r1, r2) => choices(r1) ::: choices(r2)
        case r => List(r)
      }
    def cats(re: Rx): List[Rx] =
      re match {
        case Cat(r1, r2) => cats(r1) ::: cats(r2)
        case r => List(r)
      }
    def recur(re: Rx, parens: Boolean): String =
      re match {
        case Phi => "∅"
        case Empty => ""
        case Var(x) => s"Var($x)"
        case Letter(c) => LetterSet.escape(c)
        case Letters(cs) => cs.toString
        case Star(r) => recur(r, true) + "*"
        case c @ Choice(_, _) =>
          val s = choices(c).map(recur(_, false)).mkString("|")
          if (parens) s"($s)" else s
        case c @ Cat(_, _) =>
          val s = cats(c).map(recur(_, true)).mkString
          if (parens) s"($s)" else s
      }
    "/" + recur(this, false) + "/"
  }

  //override def toString: String = scalaRepr
  override def toString: String = repr

  def scalaRepr: String = {
    def recur(re: Rx): String =
      re match {
        case Phi => "ϕ"
        case Empty => "ε"
        case Letter(c) => LetterSet.escape(c)
        case Letters(cs) => cs.toString
        case Choice(r1, r2) => s"(${recur(r1)}+${recur(r2)})"
        case Cat(r1, r2) => s"(${recur(r1)}*${recur(r2)})"
        case Star(Letter(c)) => s"$c.star"
        case Star(Letters(cs)) => s"$cs.star"
        case Star(r) => s"(${recur(r)}).star"
        case Var(x) => "$" + x.toString
      }
    recur(this)
  }

  def isSingle: Boolean =
    this match {
      case Letter(_) | Letters(_) => true
      case _ => false
    }

  def isPhi: Boolean =
    this match {
      case Phi => true
      case Empty | Letter(_) | Letters(_) | Star(_) | Var(_) => false
      case Choice(r1, r2) => r1.isPhi && r2.isPhi
      case Cat(r1, r2) => r1.isPhi || r2.isPhi
    }

  def isEmpty: Boolean =
    this match {
      case Empty => true
      case Phi | Letter(_) | Letters(_) | Star(_) | Var(_) => false
      case Choice(r1, r2) => r1.isEmpty && r2.isEmpty
      case Cat(r1, r2) => r1.isEmpty && r2.isEmpty
    }

  lazy val matchesEmpty: Boolean =
    this match {
      case Empty | Star(_) => true
      case Phi | Letter(_) | Letters(_) => false
      case Choice(r1, r2) => r1.matchesEmpty || r2.matchesEmpty
      case Cat(r1, r2) => r1.matchesEmpty && r2.matchesEmpty
      case Var(_) => sys.error("!")
    }

  def deriv(c: Char): Rx =
    Rx.choice(partialDeriv(c))

  def partialDeriv(x: Char): Set[Rx] =
    this match {
      case Phi | Empty => Set.empty
      case Letter(c) if c == x => Set(Empty)
      case Letters(cs) if cs.contains(x) => Set(Empty)
      case Letter(_) | Letters(_) => Set.empty
      case Choice(r1, r2) => r1.partialDeriv(x) | r2.partialDeriv(x)
      case Star(r) => r.partialDeriv(x).filter(_ != Phi).map(_ * this)
      case Cat(r1, r2) =>
        val s1 = r1.partialDeriv(x).map(_ * r2)
        if (r1.matchesEmpty) s1 | r2.partialDeriv(x) else s1
    }

  def resolve(x: Int): Rx = {
    def recur(r: Rx, x: Int): (List[Rx], List[Rx]) =
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
    Rx.choice(rs).star * Rx.choice(bs)
  }

  // -1 means (lhs < rhs) means (lhs subsetOf rhs)
  //  0 means (lhs = rhs) means (lhs equiv rhs)
  // +1 means (lhs > rhs) means (lhs supsersetOf rhs)
  // NaN means none of the above
  def partialCompare(rhs: Rx): Double = {

    def recur(env: Set[(Rx, Rx)], pair: (Rx, Rx)): Double =
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

          var res =
            (lhs.matchesEmpty, rhs.matchesEmpty) match {
              case (false, true) => -1.0
              case (true, false) => 1.0
              case _ => 0.0
            }

          var alpha: List[Char] = Nil
          val (ranges1, ranges2) = (lhs.firstRanges, rhs.firstRanges)
          val diffIt = LetterSet.diff(ranges1.iterator, ranges2.iterator)
          while (diffIt.hasNext) {
            diffIt.next match {
              case Diff.Both((c, _)) =>
                alpha = c :: alpha
              case Diff.Left(_) =>
                if (res < 0.0) return Double.NaN
                res = 1.0
              case Diff.Right(_) =>
                if (res > 0.0) return Double.NaN
                res = -1.0
            }
          }

          val env2 = env + pair
          val alphaIt = alpha.iterator
          while (alphaIt.hasNext && !isNaN(res)) {
            val c = alphaIt.next
            val x = recur(env2, (lhs.deriv(c), rhs.deriv(c)))
            res = acc(res, x)
          }
          res
      }

    if (lhs == rhs) 0.0 else recur(Set.empty, (lhs, rhs))
  }

  def starDepth: Int =
    this match {
      case Star(r) => r.starDepth + 1
      case Choice(r1, r2) => Integer.max(r1.starDepth, r2.starDepth)
      case Cat(r1, r2) => Integer.max(r1.starDepth, r2.starDepth)
      case Var(_) => sys.error("!")
      case _ => 0
    }
}

object Rx {

  def zero: Rx = Phi
  def phi: Rx = Phi

  def empty: Rx = Empty
  def lambda: Rx = Empty

  def apply(c: Char): Rx =
    Letter(c)

  def apply(cc: (Char, Char)): Rx = {
    val (c1, c2) = cc
    if (c1 == c2) Letter(c1)
    else Letters(LetterSet(c1 to c2))
  }

  def apply(cs: LetterSet): Rx =
    if (cs.isEmpty) Empty
    else cs.singleValue match {
      case Some(c) => Letter(c)
      case None => Letters(cs)
    }

  def apply(cs: Set[Char]): Rx =
    if (cs.size == 0) Empty
    else if (cs.size == 1) Letter(cs.head)
    else Letters(LetterSet(cs))

  def apply(s: String): Rx =
    s.foldRight(Rx.lambda)(_ *: _)

  def choice(rs: Iterable[Rx]): Rx =
    if (rs.isEmpty) Phi else rs.reduceLeft(_ + _)

  def universe(alphabet: LetterSet): Rx =
    Letters(alphabet).star

  def universe(alphabet: Set[Char]): Rx =
    universe(LetterSet(alphabet))

  case object Phi extends Rx // matches nothing
  case object Empty extends Rx // matches empty string ("")
  case class Letter(c: Char) extends Rx // single character
  case class Letters(ls: LetterSet) extends Rx // single character
  case class Choice(r1: Rx, r2: Rx) extends Rx // either
  case class Cat(r1: Rx, r2: Rx) extends Rx // concatenation
  case class Star(r: Rx) extends Rx // kleene star
  case class Var(x: Int) extends Rx // used internally

  def intersect(r1: Rx, r2: Rx): Rx = {
    def recur(cnt: Int, env: Map[(Rx, Rx), Rx], pair: (Rx, Rx)): Rx = {
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
              var alpha: List[(Char, Char)] = Nil
              val diffIt = LetterSet.diff(r1.firstRanges.iterator, r2.firstRanges.iterator)
              while (diffIt.hasNext) {
                diffIt.next match {
                  case Diff.Both(pair) => alpha = pair :: alpha
                  case _ => ()
                }
              }

              val env2 = env.updated(pair, Var(cnt))
              def f(cc: (Char, Char)): Rx =
                Rx(cc) * recur(cnt + 1, env2, (r1.deriv(cc._1), r2.deriv(cc._1)))
              val rr = Rx.choice(alpha.iterator.map(f).toList)
              val rr2 = if (r1.matchesEmpty && r2.matchesEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  def difference(r1: Rx, r2: Rx): Rx = {
    def recur(cnt: Int, env: Map[(Rx, Rx), Rx], pair: (Rx, Rx)): Rx = {
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
              var alpha: List[(Char, Char)] = Nil
              val diffIt = LetterSet.diff(r1.firstRanges.iterator, r2.firstRanges.iterator)
              while(diffIt.hasNext) {
                diffIt.next match {
                  case Diff.Both(pair) => alpha = pair :: alpha
                  case Diff.Left(pair) => alpha = pair :: alpha
                  case Diff.Right(_) => ()
                }
              }
              val env2 = env.updated(pair, Var(cnt))
              def f(cc: (Char, Char)): Rx =
                Rx(cc) * recur(cnt + 1, env2, (r1.deriv(cc._1), r2.deriv(cc._1)))
              val rr = Rx.choice(alpha.iterator.map(f).toList)
              val rr2 = if (r1.matchesEmpty && !r2.matchesEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  def xor(r1: Rx, r2: Rx): Rx = {
    def recur(cnt: Int, env: Map[(Rx, Rx), Rx], pair: (Rx, Rx)): Rx = {
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
              val alpha = LetterSet.diff(r1.firstRanges.iterator, r2.firstRanges.iterator).map(_.value)
              val env2 = env.updated(pair, Var(cnt))
              def f(cc: (Char, Char)): Rx =
                Rx(cc) * recur(cnt + 1, env2, (r1.deriv(cc._1), r2.deriv(cc._1)))
              val rr = Rx.choice(alpha.iterator.map(f).toList)
              val rr2 = if (r1.matchesEmpty ^ r2.matchesEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  // cartesian product
  def cart(xs: List[Rx], ys: List[Rx]): List[Rx] =
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
}
