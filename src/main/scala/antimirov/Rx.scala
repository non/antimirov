package antimirov

import java.lang.Double.isNaN
import java.util.regex.{Pattern => JavaPattern}
import scala.collection.mutable
import scala.util.matching.{Regex => ScalaRegex}

/**
 * Rx is a regular expression.
 */
sealed abstract class Rx { lhs =>

  import Rx._

  lazy val firstSet: List[LetterSet] =
    this match {
      case Phi => Nil
      case Empty => Nil
      case Letter(c) => LetterSet(c) :: Nil
      case Letters(cs) => cs :: Nil
      case Choice(r1, r2) =>
        LetterSet.venn(r1.firstSet, r2.firstSet).map(_.value)
      case Concat(r1, r2) if r1.acceptsEmpty =>
        LetterSet.venn(r1.firstSet, r2.firstSet).map(_.value)
      case Concat(r1, _) => r1.firstSet
      case Star(r) => r.firstSet
      case Repeat(r, _, _) => r.firstSet
      case Var(_) => sys.error("!")
    }

  def accepts(s: String): Boolean = {
    def recur(r: Rx, i: Int): Iterator[Unit] =
      if (i >= s.length) {
        if (r.acceptsEmpty) Iterator(()) else Iterator.empty
      } else {
        r.partialDeriv(s.charAt(i)).iterator.flatMap(recur(_, i + 1))
      }
    recur(this, 0).hasNext
  }

  def rejects(s: String): Boolean =
    !accepts(s)

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
    else Concat(lhs, rhs)

  def star: Rx =
    this match {
      case Phi | Empty => Empty
      case Star(_) => this
      case _ => Star(this)
    }

  def pow(k: Int): Rx = {
    def loop(term: Rx, prod: Rx, i: Int): Rx =
      if (i <= 0) Rx.empty
      else if (i == 1) term * prod
      else {
        val p = if (i % 2 == 1) term * prod else prod
        loop(term * term, p, i / 2)
      }
    loop(this, Rx.empty, k)
  }

  def repeat(n: Int): Rx =
    this match {
      case Phi | Empty => this
      case _ => Repeat(this, n, n)
    }

  def repeat(m: Int, n: Int): Rx =
    this match {
      case Phi | Empty => this
      case _ => Repeat(this, m, n)
    }

  /**
   * Attempt to put a regular expression in a canonical form.
   *
   * This is not guaranteed to be a "minimal" form (in fact it will
   * often expand the size of the regex). However, two regular
   * exprssions that are equal should have equivalent representations
   * after canonicalization is performed.
   */
  def canonical: Rx =
    Rx.canonicalize(this)

  def &(rhs: Rx): Rx =
    Rx.intersect(lhs, rhs)

  def -(rhs: Rx): Rx =
    Rx.difference(lhs, rhs)

  def ^(rhs: Rx): Rx =
    Rx.xor(lhs, rhs)

  def unary_~ : Rx =
    Rx.difference(Rx.Universe, this)

  def equiv(rhs: Rx): Boolean = {
    val derivCache = mutable.Map.empty[(Rx, Char), Rx]
    def recur(env: Set[(Rx, Rx)], pair: (Rx, Rx)): Boolean = {
      pair match {
        case (r1, r2) if r1.acceptsEmpty != r2.acceptsEmpty => false
        case (r1, r2) if r1.isPhi != r2.isPhi => false
        case _ if env(pair) => true
        case (r1, r2) =>
          val env2 = env + pair
          val alpha = LetterSet.venn(r1.firstSet, r2.firstSet)
          alpha.forall(_.isBoth) && alpha.forall { d =>
            val c = d.value.minOption.get
            val d1 = derivCache.getOrElseUpdate((r1, c), r1.deriv(c))
            val d2 = derivCache.getOrElseUpdate((r2, c), r2.deriv(c))
            recur(env2, (d1, d2))
          }
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
        case Concat(r1, r2) => cats(r1) ::: cats(r2)
        case r => List(r)
      }
    def recur(re: Rx, parens: Boolean): String =
      re match {
        case Phi => "∅"
        case Empty => ""
        case Var(x) => s"Var($x)"
        case Letter(c) => Chars.escape(c)
        case Letters(cs) if cs.isFull => "."
        case Letters(cs) =>
          if (cs.size <= 32768) cs.repr
          else {
            val ccs = ~cs
            ccs.ranges.map {
              case (x, y) if x == y => LetterSet.escape(x)
              case (x, y) => s"${LetterSet.escape(x)}-${LetterSet.escape(y)}"
            }.mkString("[^", "", "]")
          }
        case Star(r) => recur(r, true) + "*"
        case Repeat(r, m, n) if m == n => recur(r, true) + s"{$m}"
        case Repeat(r, m, n) => recur(r, true) + s"{$m,$n}"
        case c @ Choice(_, _) =>
          val s = choices(c).map(recur(_, false)).mkString("|")
          if (parens) s"($s)" else s
        case c @ Concat(_, _) =>
          val s = cats(c).map(recur(_, true)).mkString
          if (parens) s"($s)" else s
      }
    recur(this, false)
  }

  override def toString: String = repr

  def scalaRepr: String = {
    def recur(re: Rx): String =
      re match {
        case Phi => "ϕ"
        case Empty => "ε"
        case Letter(c) => s"Rx('${Chars.escape(c)}')"
        case Letters(cs) => "Rx.parse(\"" + cs.toString + "\")"
        case Choice(r1, r2) => s"(${recur(r1)}+${recur(r2)})"
        case Concat(r1, r2) => s"(${recur(r1)}*${recur(r2)})"
        case Star(r) => s"${recur(r)}.star"
        case Repeat(r, m, n) if m == n => s"${recur(r)}.repeat($m)"
        case Repeat(r, m, n) => s"${recur(r)}.repeat($m,$n)"
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
      case Repeat(r, _, _) => r.isPhi
      case Choice(r1, r2) => r1.isPhi && r2.isPhi
      case Concat(r1, r2) => r1.isPhi || r2.isPhi
    }

  def isEmpty: Boolean =
    this match {
      case Empty => true
      case Phi | Letter(_) | Letters(_) | Star(_) | Repeat(_, _, _) | Var(_) => false
      case Choice(r1, r2) => r1.isEmpty && r2.isEmpty
      case Concat(r1, r2) => r1.isEmpty && r2.isEmpty
    }

  lazy val acceptsEmpty: Boolean =
    this match {
      case Empty | Star(_) => true
      case Phi | Letter(_) | Letters(_) => false
      case Repeat(r, m, _) => m == 0 || r.acceptsEmpty
      case Choice(r1, r2) => r1.acceptsEmpty || r2.acceptsEmpty
      case Concat(r1, r2) => r1.acceptsEmpty && r2.acceptsEmpty
      case Var(_) => sys.error("!")
    }

  def rejectsEmpty: Boolean =
    !acceptsEmpty

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
      case Repeat(r, m, n) =>
        val s1 = r.partialDeriv(x).filter(_ != Phi)
        if (s1.isEmpty) {
          Set.empty
        } else {
          val rr = if (n <= 1) Empty else Repeat(r, Integer.max(0, m - 1), n - 1)
          s1.map(_ * rr)
        }
      case Concat(r1, r2) =>
        val s1 = r1.partialDeriv(x).map(_ * r2)
        if (r1.acceptsEmpty) s1 | r2.partialDeriv(x) else s1
    }

  def resolve(x: Int): Rx = {

    // cartesian product
    def cart(xs: List[Rx], ys: List[Rx]): List[Rx] =
      for { x <- xs; y <- ys } yield x * y

    def recur(r: Rx, x: Int): (List[Rx], List[Rx]) =
      r match {
        case v @ Var(y) =>
          if (y == x) (List(Empty), Nil) else (Nil, List(v))
        case Concat(r1, r2) =>
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

    val derivCache = mutable.Map.empty[(Rx, Char), Rx]

    def recur(env: Set[(Rx, Rx)], pair: (Rx, Rx)): Double =
      pair match {
        case (Phi, rhs) =>
          if (rhs.isPhi) 0.0 else -1.0
        case (lhs, Phi) =>
          if (lhs.isPhi) 0.0 else 1.0
        case (Empty, rhs) =>
          if (rhs.isEmpty) 0.0
          else if (rhs.acceptsEmpty) -1.0
          else Double.NaN
        case (lhs, Empty) =>
          if (lhs.isEmpty) 0.0
          else if (lhs.acceptsEmpty) 1.0
          else Double.NaN
        case _ if env(pair) =>
          0.0
        case (lhs, rhs) =>

          var res = (lhs.acceptsEmpty, rhs.acceptsEmpty) match {
            case (false, true) => -1.0
            case (true, false) => 1.0
            case _ => 0.0
          }

          val alpha = LetterSet.venn(lhs.firstSet, rhs.firstSet)
          val diffIt = alpha.iterator
          while (diffIt.hasNext) {
            diffIt.next match {
              case Diff.Left(_) =>
                if (res < 0.0) return Double.NaN
                res = 1.0
              case Diff.Right(_) =>
                if (res > 0.0) return Double.NaN
                res = -1.0
              case _ => ()
            }
          }

          val env2 = env + pair
          val alphaIt = alpha.iterator
          while (alphaIt.hasNext && !isNaN(res)) {
            val c = alphaIt.next.value.minOption.get
            val d1 = derivCache.getOrElseUpdate((lhs, c), lhs.deriv(c))
            val d2 = derivCache.getOrElseUpdate((rhs, c), rhs.deriv(c))
            val x = recur(env2, (d1, d2))
            res = acc(res, x)
          }
          res
      }

    if (lhs == rhs) 0.0 else recur(Set.empty, (lhs, rhs))
  }

  def toJava: JavaPattern =
    JavaPattern.compile(repr)

  def toScala: ScalaRegex =
    new ScalaRegex(repr)
}

object Rx {

  def zero: Rx = Phi
  def phi: Rx = Phi

  def empty: Rx = Empty
  def lambda: Rx = Empty

  val dot: Rx = Rx.Letters(LetterSet.Full)

  def parse(s: String): Rx =
    Parser.parse(s)

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

  val Universe: Rx =
    closure(LetterSet.Full)

  def closure(alphabet: LetterSet): Rx =
    Letters(alphabet).star

  def closure(alphabet: Set[Char]): Rx =
    closure(LetterSet(alphabet))

  case object Phi extends Rx // matches nothing
  case object Empty extends Rx // matches empty string ("")
  case class Letter(c: Char) extends Rx // single character
  case class Letters(ls: LetterSet) extends Rx // single character, one of a set
  case class Choice(r1: Rx, r2: Rx) extends Rx // either
  case class Concat(r1: Rx, r2: Rx) extends Rx // concatenation
  case class Repeat(r: Rx, m: Int, n: Int) extends Rx // repetition, n > 0, n >= m
  case class Star(r: Rx) extends Rx // kleene star
  case class Var(x: Int) extends Rx // used internally

  def canonicalize(r: Rx): Rx = {
    val derivCache = mutable.Map.empty[(Rx, Char), Rx]
    def recur(cnt: Int, env: Map[Rx, Rx], r: Rx): Rx = {
      r match {
        case Phi => Phi
        case Empty => Empty
        case Star(r) => Star(r.canonical)
        case _ =>
          env.get(r) match {
            case Some(res) =>
              res
            case None =>
              val env2 = env.updated(r, Var(cnt))
              def f(cs: LetterSet): Rx = {
                val c = cs.minOption.get
                val d = derivCache.getOrElseUpdate((r, c), r.deriv(c))
                Rx(cs) * recur(cnt + 1, env2, d)
              }
              val set = r.firstSet.sortBy(s => (s.minOption, s.maxOption))
              val r1 = Rx.choice(set.map(f))
              val r2 = if (r.acceptsEmpty) r1 + Empty else r1
              r2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, r)
  }

  def intersect(r1: Rx, r2: Rx): Rx = {
    val derivCache = mutable.Map.empty[(Rx, Char), Rx]
    def recur(cnt: Int, env: Map[(Rx, Rx), Rx], pair: (Rx, Rx)): Rx = {
      val (r1, r2) = pair
      pair match {
        case (Phi, _) | (_, Phi) => Phi
        case (Empty, r2) => if (r2.acceptsEmpty) Empty else Phi
        case (r1, Empty) => if (r1.acceptsEmpty) Empty else Phi
        case (r1, r2) =>
          env.get(pair) match {
            case Some(res) =>
              res
            case None =>
              val alpha = LetterSet.venn(r1.firstSet, r2.firstSet).collect {
                case Diff.Both(cs) => cs
              }
              val env2 = env.updated(pair, Var(cnt))
              def f(cs: LetterSet): Rx = {
                val c = cs.minOption.get
                val d1 = derivCache.getOrElseUpdate((r1, c), r1.deriv(c))
                val d2 = derivCache.getOrElseUpdate((r2, c), r2.deriv(c))
                Rx(cs) * recur(cnt + 1, env2, (d1, d2))
              }
              val rr = Rx.choice(alpha.map(f))
              val rr2 = if (r1.acceptsEmpty && r2.acceptsEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  def difference(r1: Rx, r2: Rx): Rx = {
    val derivCache = mutable.Map.empty[(Rx, Char), Rx]
    def recur(cnt: Int, env: Map[(Rx, Rx), Rx], pair: (Rx, Rx)): Rx = {
      val (r1, r2) = pair
      pair match {
        case (Phi, _) => Phi
        case (Empty, r2) => if (r2.acceptsEmpty) Phi else Empty
        case (_, Phi) => r1
        case (r1, r2) =>
          env.get(pair) match {
            case Some(res) =>
              res
            case None =>
              val alpha = LetterSet.venn(r1.firstSet, r2.firstSet).collect {
                case Diff.Both(cs) => cs
                case Diff.Left(cs) => cs
              }
              val env2 = env.updated(pair, Var(cnt))
              def f(cs: LetterSet): Rx = {
                val c = cs.minOption.get
                val d1 = derivCache.getOrElseUpdate((r1, c), r1.deriv(c))
                val d2 = derivCache.getOrElseUpdate((r2, c), r2.deriv(c))
                Rx(cs) * recur(cnt + 1, env2, (d1, d2))
              }
              val rr = Rx.choice(alpha.map(f))
              val rr2 = if (r1.acceptsEmpty && !r2.acceptsEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }

  def xor(r1: Rx, r2: Rx): Rx = {
    val derivCache = mutable.Map.empty[(Rx, Char), Rx]
    def recur(cnt: Int, env: Map[(Rx, Rx), Rx], pair: (Rx, Rx)): Rx = {
      val (r1, r2) = pair
      pair match {
        case (r1, Phi) => r1
        case (Phi, r2) => r2
        case (Empty, r2) if !r2.acceptsEmpty => r2 + Empty
        case (r1, Empty) if !r1.acceptsEmpty => r1 + Empty
        case (r1, r2) =>
          env.get(pair) match {
            case Some(res) =>
              res
            case None =>
              val alpha = LetterSet.venn(r1.firstSet, r2.firstSet).map(_.value)
              val env2 = env.updated(pair, Var(cnt))
              def f(cs: LetterSet): Rx = {
                val c = cs.minOption.get
                val d1 = derivCache.getOrElseUpdate((r1, c), r1.deriv(c))
                val d2 = derivCache.getOrElseUpdate((r2, c), r2.deriv(c))
                Rx(cs) * recur(cnt + 1, env2, (d1, d2))
              }
              val rr = Rx.choice(alpha.map(f))
              val rr2 = if (r1.acceptsEmpty ^ r2.acceptsEmpty) rr + Empty else rr
              rr2.resolve(cnt)
          }
      }
    }
    recur(1, Map.empty, (r1, r2))
  }
}
