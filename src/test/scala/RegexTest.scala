package regsym

import org.scalacheck.{Gen, Prop, Properties}

import Prop.{forAllNoShrink => forAll}
import Regex.{Empty, Impossible}

object RegexTest extends Properties("RegexTest") {

  case class Syms(list: List[Sym]) {
    def ++(rhs: Syms): Syms = Syms(list ::: rhs.list)
  }

  val genSym: Gen[Sym] =
    Gen.oneOf(Gen.const(Sym.A), Gen.const(Sym.B))

  val genInput: Gen[Syms] =
    Gen.listOf(genSym).map(Syms(_))

  def symsFromRegex(r: Regex): Option[Gen[Syms]] =
    r match {
      case Impossible =>
        None
      case Empty =>
        Some(Gen.const(Syms(Nil)))
      case Regex.Literal(sym) =>
        Some(Gen.const(Syms(sym :: Nil)))
      case Regex.Or(x, y) =>
        (symsFromRegex(x), symsFromRegex(y)) match {
          case (Some(gx), Some(gy)) => Some(Gen.oneOf(gx, gy))
          case (some @ Some(_), None) => some
          case (None, some @ Some(_)) => some
          case (None, None) => None
        }
      case Regex.Then(x, y) =>
        for {
          gx <- symsFromRegex(x)
          gy <- symsFromRegex(y)
        } yield for {
          sx <- gx
          sy <- gy
        } yield sx ++ sy
      case r @ Regex.Star(x) =>
        val ge = Gen.const(Syms(Nil))
        symsFromRegex(x) match {
          case None =>
            Some(ge)
          case Some(gx) =>
            lazy val g: Gen[Syms] =
              Gen.lzy(Gen.oneOf(
                ge,
                for { sx <- gx; s <- g } yield sx ++ s))
            Some(g)
        }
    }

  def genRecur(depth: Int, f: Int => Gen[Regex]): Gen[Regex] =
    if (depth <= 0) {
      lazy val g: Gen[Regex] = Gen.lzy(Gen.frequency(
        1 -> Regex.Literal(Sym.A),
        1 -> Regex.Literal(Sym.B),
        1 -> (for { x <- genSym; y <- g } yield x *: y)))
      g
    } else {
      val g0 = f(0)
      val g = f(depth - 1)
      Gen.frequency(
        1 -> Impossible,
        1 -> Empty,
        10 -> Regex.Literal(Sym.A),
        10 -> Regex.Literal(Sym.B),
        20 -> (for { x <- genSym; y <- g } yield x *: y),
        10 -> (for { x <- g; y <- g } yield x + y),
        5 -> (for { x <- g } yield x.star))
    }

  lazy val genRegex: Gen[Regex] = {
    lazy val f: Int => Gen[Regex] =
      (n: Int) => genRecur(n, f)
    f(4)
  }

  val genRegexAndSyms: Gen[(Regex, List[Syms])] =
    genRegex.flatMap { r =>
      symsFromRegex(r) match {
        case None => Gen.const((r, Nil))
        case Some(g) => Gen.listOf(g).map(lst => (r, lst))
      }
    }

  property("regex accepts syms") =
    forAll(genRegexAndSyms) { case (r, lst) =>
      val dfa = r.toDfa
      lst.forall { case Syms(s) => dfa.accepts(s) }
    }

  property("regex union accepts") =
    forAll(genRegexAndSyms, genRegexAndSyms) { case ((r1, lst1), (r2, lst2)) =>
      val dfa = (r1 + r2).toDfa
      (lst1 ::: lst2).forall { case Syms(s) => dfa.accepts(s) }
    }

  property("regex not accepts") =
    forAll(genRegexAndSyms) { case (r, lst) =>
      val dfa = (~r).toDfa
      lst.forall { case Syms(s) => !dfa.accepts(s) }
    }

  val genIntersect: Gen[(Regex, Regex, Regex, List[Syms])] =
    genRegex.flatMap { r1 =>
      genRegex.flatMap { r2 =>
        val r3 = r1 & r2
        symsFromRegex(r3) match {
          case None => Gen.const((r1, r2, r3, Nil))
          case Some(g) => Gen.listOf(g).map(lst => (r1, r2, r3, lst))
        }
      }
    }

  property("regex intersection accepts") =
    forAll(genIntersect) { case (r1, r2, r3, lst) =>
      val (dfa1, dfa2) = (r1.toDfa, r2.toDfa)
      lst.forall { case Syms(s) => dfa1.accepts(s) && dfa2.accepts(s) }
    }

  property("x | y = y | x") =
    forAll(genRegex, genRegex) { (x, y) =>
      (x + y) equiv (y + x)
    }

  property("x & y = y & x") =
    forAll(genRegex, genRegex) { (x, y) =>
      (x & y) equiv (y & x)
    }

  property("~~x = x") =
    forAll(genRegex) { x =>
      ~(~x) equiv x
    }

  property("x** = x*") =
    forAll(genRegex) { x =>
      x.star.star equiv x.star
    }

  property("xx*|ε = x*") =
    forAll(genRegex) { x =>
      (Empty + (x * x.star)) equiv x.star
    }

  property("(x | y)* > (x* | y*)") =
    forAll(genRegex, genRegex) { (x, y) =>
      ((x + y).star) supersetOf (x.star + y.star)
    }

  property("x∅ = ∅x = ∅") =
    forAll(genRegex) { x =>
      ((x * Impossible) == Impossible) && ((Impossible * x) == Impossible)
    }

  property("x * (y + z) = (x * y) + (x * z)") =
    forAll(genRegex, genRegex, genRegex) { (x, y, z) =>
      (x * (y + z)) equiv ((x * y) + (x * z))
    }

  property("(x + y) * z = (x * z) + (y * z)") =
    forAll(genRegex, genRegex, genRegex) { (x, y, z) =>
      ((x + y) * z) equiv ((x * z) + (y * z))
    }

  property("x <= y -> y accepts x") =
    forAll(genRegexAndSyms, genRegexAndSyms) { case ((x, lstx), (y, lsty)) =>
      val (lst, dfa) =
        if (x subsetOf y) (lstx, y.toDfa)
        else if (y subsetOf x) (lsty, x.toDfa)
        else (Nil, x.toDfa)
      lst.forall { case Syms(s) => dfa.accepts(s) }
    }
}
