package antimirov

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.typelevel.claimant.Claim
import scala.util.Try

import Arbitrary.arbitrary
import Prop.{forAllNoShrink => forAll}

abstract class LetterMapTesting(name: String) extends Properties(name) with AlphaTesting {

  override def overrideParameters(ps: Test.Parameters): Test.Parameters =
    ps.withMinSuccessfulTests(100)

  property("x.nonEmpty = !x.isEmpty") =
    forAll { (x: LetterMap[Int]) =>
      Claim(x.nonEmpty == !x.isEmpty)
    }

  property("x ++ empty = empty ++ x = x") =
    forAll { (x: LetterMap[Int]) =>
      val e = LetterMap.empty[Int]
      Claim(((x ++ e) == x) && ((e ++ x) == x))
    }

  property("++ is associative") =
    forAll { (x: LetterMap[Int], y: LetterMap[Int], z: LetterMap[Int]) =>
      val lhs = (x ++ y) ++ z
      val rhs = x ++ (y ++ z)
      Claim(lhs == rhs)
    }

  property("merge is associative") =
    forAll { (x: LetterMap[Int], y: LetterMap[Int], z: LetterMap[Int]) =>
      val f: (Int, Int) => Int = _ + _
      val lhs = x.merge(y)(f).merge(z)(f)
      val rhs = x.merge(y.merge(z)(f))(f)
      Claim(lhs == rhs)
    }

  property("(x == y) => (x.toString == y.toString)") =
    forAll { (x: LetterMap[Int], y: LetterMap[Int]) =>
      Claim(x != y || x.toString == y.toString)
    }

  property("x.mapValues(f compose g) = x.mapValues(f).mapValues(g)") =
    forAll { (x: LetterMap[Int], f: Int => Long, g: Long => Int) =>
      val lhs = x.mapValues(f andThen g)
      val rhs = x.mapValues(f).mapValues(g)
      Claim(lhs == rhs)
    }

  property("(x == y) => (x.toString == y.toString)") =
    forAll { (x: LetterMap[Int], a: Alpha) =>
      val c = a.value
      Claim(x.contains(c) == x.keySet.contains(c))
    }

  property("get = contains ~ apply") =
    forAll { (x: LetterMap[Int], a: Alpha) =>
      val c = a.value
      val lhs = x.get(c)
      val rhs = if (x.contains(c)) Some(x(c)) else None
      Claim(lhs == rhs)
    }

  property("get = try apply") =
    forAll { (x: LetterMap[Int], a: Alpha) =>
      val c = a.value
      val lhs = x.get(c)
      val rhs = Try(x(c)).toOption
      Claim(lhs == rhs)
    }

  property("(x = y) -> (x.hashCode = y.hashCode)") =
    forAll { (x: LetterMap[Int], y: LetterMap[Int]) =>
      Claim((x != y) || (x.hashCode == y.hashCode))
    }

  property("x + (c, 3) != x + (c, 4)") =
    forAll { (x: LetterMap[Int], c: Char) =>
      Claim((x ++ LetterMap(c, 3)) != (x ++ LetterMap(c, 4)))
    }

  property("LetterMap[Int] != Map[Char, Int]") =
    forAll { (x: LetterMap[Int], y: Map[Char, Int]) =>
      Claim(x != y)
    }
}

object LetterMapTestAlpha extends LetterMapTesting("LetterMap(alpha)") {
  def genChar: Gen[Char] = Gen.alphaChar
}

object LetterMapTestFull extends LetterMapTesting("LetterMap(full)") {
  def genChar: Gen[Char] = arbitrary[Char]
}
