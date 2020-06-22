package antimirov

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.typelevel.claimant.Claim
import scala.reflect.ClassTag
import scala.util.Try

import Arbitrary.arbitrary
import Prop.{forAllNoShrink => forAll}

trait AlphaTesting {
  def genChar: Gen[Char]

  val genLetterSet: Gen[LetterSet] =
    Gen.buildableOf[Set[Char], Char](genChar).map(LetterSet(_))

  implicit val arbitraryLetterSet: Arbitrary[LetterSet] =
    Arbitrary(genLetterSet)

  def genLetterMap[A: ClassTag](ga: Gen[A]): Gen[LetterMap[A]] = {
    val genPair = Gen.zip(genLetterSet, ga)
    val genAtom = genPair.map { case (cs, a) => LetterMap(cs, a) }
    for {
      n <- Gen.choose(1, 5)
      xs <- Gen.listOfN(n, genAtom)
    } yield xs.foldLeft(LetterMap.empty[A])(_ ++ _)
  }

  implicit def arbitraryLetterMap[A: Arbitrary: ClassTag]: Arbitrary[LetterMap[A]] =
    Arbitrary(genLetterMap(arbitrary[A]))

  case class Alpha(value: Char)

  object Alpha {
    implicit val arbitraryAlpha: Arbitrary[Alpha] =
      Arbitrary(genChar.map(Alpha(_)))
  }
}

abstract class LetterSetTesting(name: String) extends Properties(name) with AlphaTesting {

  override def overrideParameters(ps: Test.Parameters): Test.Parameters =
    ps.withMinSuccessfulTests(100)

  property("size and contains (alpha)") =
    forAll { (cs: List[Alpha]) =>
      val cset = cs.map(_.value).toSet
      val lset = LetterSet(cset)
      Claim(lset.size == cset.size) && Claim(cset.forall(lset.contains))
    }

  property("contains = apply") =
    forAll { (x: LetterSet, c: Char) =>
      Claim(x(c) == x.contains(c))
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

  property("(x partialCompare y) ~ (x & y)") =
    forAll { (x: LetterSet, y: LetterSet) =>
      val c = x partialCompare y
      val xy = x & y
      if      (c < 0.0)  Claim(xy == x && xy != y)
      else if (c > 0.0)  Claim(xy != x && xy == y)
      else if (c == 0.0) Claim(xy == x && xy == y)
      else               Claim(xy != x && xy != y)
    }

  property("regression") = {
    // x = [ce-fh-ins-tv-xz]
    // y = [Pa-eg-hkm-np-svz]
    val x = LetterSet("cefhinstvwxz".toSet)
    val y = LetterSet("Pabcdeghkmnpqrsvz".toSet)
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

  property("LetterSet != Set") =
    forAll { (x: LetterSet, y: Set[Char]) =>
      Claim(x != y)
    }

  property("forall") =
    forAll { (set: Set[Char], p: Char => Boolean) =>
      val ls = LetterSet(set)
      Claim(ls.forall(p) == set.forall(p))
    }

  property("minOption") =
    forAll { (set: Set[Char]) =>
      val ls = LetterSet(set)
      if (set.isEmpty) {
        Claim(ls.minOption == None)
      } else {
        Claim(ls.minOption == Some(set.min))
      }
    }

  property("maxOption") =
    forAll { (set: Set[Char]) =>
      val ls = LetterSet(set)
      if (set.isEmpty) {
        Claim(ls.maxOption == None)
      } else {
        Claim(ls.maxOption == Some(set.max))
      }
    }

  property("isSingleton") =
    forAll { (set: Set[Char]) =>
      val ls = LetterSet(set)
      Claim(ls.isSingleton == (set.size == 1))
    }

  property("intersects") =
    forAll { (x: LetterSet, y: LetterSet) =>
      Claim((x intersects y) == (x & y).nonEmpty)
    }

  property("get") =
    forAll { (x: LetterSet, nn0: Int, nn1: Int) =>
      val (n0, n1) = (nn0 & 0x7fffffff, nn1 & 0x7fffffff)
      val (i, j) = if (n0 <= n1) (n0, n1) else (n1, n0)
      if (j >= x.size) {
        Claim(Try(x.get(j)).isFailure)
      } else {
        Claim(x.get(i) <= x.get(j))
      }
    }

  property("CharSet(x to y) = CharSet(x until (y + 1))") =
    forAll { (c1: Char, c2: Char) =>
      val (x, y) = if (c1 <= c2) (c1, c2) else (c2, c1)
      if (y == Char.MaxValue) Prop(true)
      else Claim(LetterSet(x to y) == LetterSet(x until (y + 1).toChar))
    }

  // TODO: test venn
}

object LetterSetTestAlpha extends LetterSetTesting("LetterSet(alpha)") {
  def genChar: Gen[Char] = Gen.alphaChar
}

object LetterSetTestFull extends LetterSetTesting("LetterSet(full)") {
  def genChar: Gen[Char] = arbitrary[Char]
}
