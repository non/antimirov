package antimirov

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Arbitrary.arbitrary
import Prop.{forAllNoShrink => forAll}

abstract class LetterSetTesting(name: String) extends Properties(name) {

  def genChar: Gen[Char]

  override def overrideParameters(ps: Test.Parameters): Test.Parameters =
    ps.withMinSuccessfulTests(100)

  implicit val arbitraryLetterSet: Arbitrary[LetterSet] = {
    val genSet = Gen.buildableOf[Set[Char], Char](genChar)
    Arbitrary(genSet.map(LetterSet(_)))
  }

  case class Alpha(value: Char)

  object Alpha {
    implicit val arbitraryAlpha: Arbitrary[Alpha] =
      Arbitrary(genChar.map(Alpha(_)))
  }

  property("size and contains (alpha)") =
    forAll { (cs: List[Alpha]) =>
      val cset = cs.map(_.value).toSet
      val lset = LetterSet(cset)
      Claim(lset.size == cset.size) && Claim(cset.forall(lset.contains))
    }

  property("size and contains (full)") = {
    val gcs = Gen.listOf(Gen.choose(Char.MinValue, Char.MaxValue))
    forAll(gcs) { (cs: List[Char]) =>
      val cset = cs.toSet
      val lset = LetterSet(cset)
      Claim(lset.size == cset.size) && Claim(cset.forall(lset.contains))
    }
  }

  property("equals and hashCode") =
    forAll { (set1: LetterSet, set2: LetterSet) =>
      if (set1 == set2) Claim(set1.hashCode == set2.hashCode)
      else Claim(set1.hashCode != set2.hashCode)
    }

  property("(x == y) = (x.toString == y.toString") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x == y) == (s"$x" == s"$y"))
    }

  property("(set ± a - a) = (set - a)") =
    forAll { (set: LetterSet, a: Alpha) =>
      val c = a.value
      Claim(
        ((set + c) - c) == (set - c) &&
        ((set - c) - c) == (set - c))
    }

  property("(set ± a + a) = (set + a)") =
    forAll { (set: LetterSet, a: Alpha) =>
      val c = a.value
      Claim(
        ((set - c) + c) == (set + c) &&
        ((set + c) + c) == (set + c))
    }

  property("(set + x + y) = (set + y + x)") =
    forAll { (set: LetterSet, a1: Alpha, a2: Alpha) =>
      val (x, y) = (a1.value, a2.value)
      Claim((set + x + y) == (set + y + x))
    }

  property("~~set = set") =
    forAll { (set: LetterSet) =>
      Claim(~(~set) == set)
    }

  property("(~set).contains(a) != set.contains(a)") =
    forAll { (set: LetterSet, a: Alpha) =>
      val c = a.value
      Claim((~set).contains(c) != set.contains(c))
    }

  property("~(set ± a) = ~set ∓ a") =
    forAll { (set: LetterSet, a: Alpha) =>
      val c = a.value
      Claim(
        (~(set + c) == (~set - c)) &&
        (~(set - c) == (~set + c)))
    }

  property("set.containsRange(r) = r.forall(set.contains)") =
    forAll { (set: LetterSet, c1: Char, c2: Char) =>
      if (c1 <= c2) {
        Claim(set.containsRange(c1, c2) == (c1 to c2).forall(set.contains))
      } else {
        Claim(set.containsRange(c2, c1) == (c2 to c1).forall(set.contains))
      }
    }

  property("(x partialCompare y) = -(y partialCompare x)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val lhs = x partialCompare y
      val rhs = y partialCompare x
      if (lhs.isNaN) Claim(!(rhs < 0.0 || rhs == 0.0 || rhs >= 0.0))
      else Claim(lhs == -rhs)
    }

  property("(x = y) = ((x partialCompare y) = 0)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x == y) == ((x partialCompare y) == 0.0))
    }

  propertyWithSeed("(x partialCompare y) ~ (x & y)", Some("syDDf3NPvHHdAyAA7meKDmlTN9gKZLbNx7vsO54nA_O=")) =
    forAll { (x: LetterSet, y: LetterSet) =>
      val c = x partialCompare y
      val xy = x & y
      if      (c < 0.0)  Claim(xy == x && xy != y)
      else if (c > 0.0)  Claim(xy != x && xy == y)
      else if (c == 0.0) Claim(xy == x && xy == y)
      else               Claim(xy != x && xy != y)
    }

  property("x & 0 = 0") =
    forAll { (x: LetterSet) =>
      Claim((x & LetterSet.Empty) == LetterSet.Empty)
    }

  property("x & 1 = x") =
    forAll { (x: LetterSet) =>
      Claim((x & LetterSet.Full) == x)
    }

  property("x & x = x") =
    forAll { (x: LetterSet) =>
      Claim((x & x) == x)
    }

  property("x & (~x) = 0") =
    forAll { (x: LetterSet) =>
      Claim((x & ~x) == LetterSet.Empty)
    }

  property("(x & y) = (y & x)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x & y) == (y & x))
    }

  property("((x & y) & z) = (x & (y & z))") =
    forAll { (x: LetterSet, y: LetterSet, z: LetterSet) =>
      Claim(((x & y) & z) == (x & (y & z)))
    }

  property("(x & y & y) = (x & y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x & y & y) == (x & y))
    }

  property("(x & (y - a)) = (y & x) - a") =
    forAll { (x: LetterSet, y: LetterSet, a: Alpha) =>
      val c = a.value
      Claim((x & (y - c)) == ((y & x) - c))
    }

  property("(x & y).size <= min(x.size, y.size)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x & y).size <= Integer.min(x.size, y.size))
    }

  property("(x & y).contains(a) = x.contains(a) && y.contains(a)") =
    forAll { (x: LetterSet, y: LetterSet, a: Alpha) =>
      val c = a.value
      Claim((x & y).contains(c) == (x.contains(c) && y.contains(c)))
    }

  property("x | 0 = x") =
    forAll { (x: LetterSet) =>
      Claim((x | LetterSet.Empty) == x)
    }

  property("x | 1 = 1") =
    forAll { (x: LetterSet) =>
      Claim((x | LetterSet.Full) == LetterSet.Full)
    }

  property("x | x = x") =
    forAll { (x: LetterSet) =>
      Claim((x | x) == x)
    }

  property("x | (~x) = 1") =
    forAll { (x: LetterSet) =>
      Claim((x | ~x) == LetterSet.Full)
    }

  property("(x | y) = (y | x)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x | y) == (y | x))
    }

  property("((x | y) | z) = (x | (y | z))") =
    forAll { (x: LetterSet, y: LetterSet, z: LetterSet) =>
      Claim(((x | y) | z) == (x | (y | z)))
    }

  property("(x | y | y) = (x | y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x | y | y) == (x | y))
    }

  property("(x | (y + a)) = (y | x) + a") =
    forAll { (x: LetterSet, y: LetterSet, a: Alpha) =>
      val c = a.value
      Claim((x | (y + c)) == ((y | x) + c))
    }

  property("(x | y).size >= max(x.size, y.size)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x | y).size >= Integer.max(x.size, y.size))
    }

  property("(x | y).contains(a) = x.contains(a) || y.contains(a)") =
    forAll { (x: LetterSet, y: LetterSet, a: Alpha) =>
      val c = a.value
      Claim((x | y).contains(c) == (x.contains(c) || y.contains(c)))
    }

  property("(x + a) = (x | LetterSet(a))") =
    forAll { (x: LetterSet, a: Alpha) =>
      val c = a.value
      Claim((x + c) == (x | LetterSet(c)))
    }

  property("~(~x & ~y) = x | y") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim(~((~x) & (~y)) == (x | y))
    }

  property("~(~x | ~y) = x & y") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim(~((~x) & (~y)) == (x | y))
    }

  property("x & (y | z) = (x & y) | (x & z)") =
    forAll { (x: LetterSet, y: LetterSet, z: LetterSet) =>
      Claim((x & (y | z)) == ((x & y) | (x & z)))
    }

  property("x | (y & z) = (x | y) & (x | z)") =
    forAll { (x: LetterSet, y: LetterSet, z: LetterSet) =>
      Claim((x | (y & z)) == ((x | y) & (x | z)))
    }

  property("~1 = 0") =
    Claim(~LetterSet.Full == LetterSet.Empty)

  property("~0 = 1") =
    Claim(~LetterSet.Empty == LetterSet.Full)

  def trans(x: LetterSet, y: LetterSet)(p: Diff[(Char, Char)] => Boolean): LetterSet =
    LetterSet.diff(x, y)
      .filter(p)
      .map(d => LetterSet(d.value._1 to d.value._2))
      .foldLeft(LetterSet.empty)(_ | _)

  property("diff(x, y) ~ x") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val z = trans(x, y)(!_.isRight)
      Claim(z == x)
    }

  property("diff(x, y) ~ (x & y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val z = trans(x, y)(_.isBoth)
      Claim(z == (x & y))
    }

  property("diff(x, y) ~ (x | y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val z = trans(x, y)(_ => true)
      Claim(z == (x | y))
    }

  property("diff(x, y) ~ (x ^ y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val z = trans(x, y)(!_.isBoth)
      Claim(z == (x ^ y))
    }

  property("diff(x, y) ~ (x -- y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val z = trans(x, y)(_.isLeft)
      Claim(z == (x -- y))
    }

  property("diff(diff(w, x), diff(y, z)) ~ (w | x | y | z)") =
    forAll { (w: LetterSet, x: LetterSet, y: LetterSet, z: LetterSet) =>
      val lhs = LetterSet.diff(
        LetterSet.diff(w, x).map(_.value),
        LetterSet.diff(y, z).map(_.value))
        .map(d => LetterSet(d.value._1 to d.value._2))
        .foldLeft(LetterSet.Empty)(_ | _)
      val rhs = (w | x | y | z)
      Claim(lhs == rhs)
    }
}

object LetterSetTestAlpha extends LetterSetTesting("LetterSet(alpha)") {
  def genChar: Gen[Char] = Gen.alphaChar
}

object LetterSetTestFull extends LetterSetTesting("LetterSet(full)") {
  def genChar: Gen[Char] = arbitrary[Char]
}
