package antimirov

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}

object SizeTest extends Properties("SizeTest") { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(1000)

  lazy val genSize: Gen[Size] =
    Gen.frequency(
      8 -> Gen.choose(0L, 1000000L).map(n => Size(n)),
      8 -> Gen.choose(0L, Long.MaxValue).map(n => Size(n)),
      3 -> Gen.lzy(for { x <- genSize; y <- genSize } yield x + y),
      2 -> Gen.lzy(for { x <- genSize; y <- genSize } yield x * y),
      1 -> Gen.choose(0L, Long.MaxValue).map(n => Size(BigInt(n))),
      1 -> Gen.const(Size.Unbounded))

  implicit val arbitrarySize: Arbitrary[Size] =
    Arbitrary(genSize)

  property("((x + y) + z) = (x + (y + z))") =
    forAll { (x: Size, y: Size, z: Size) =>
      Claim(((x + y) + z) == (x + (y + z)))
    }

  property("((x * y) * z) = (x * (y * z))") =
    forAll { (x: Size, y: Size, z: Size) =>
      Claim(((x * y) * z) == (x * (y * z)))
    }

  property("x.pow(k) = x * x * ... * x") =
    forAll(genSize, Gen.choose(0, 10)) { (x: Size, k: Int) =>
      val lhs = x.pow(k)
      val rhs = (1 to k).map(_ => x).foldLeft(Size.One)(_ * _)
      Claim(lhs == rhs)
    }

  property("(x compare y) = -1 * (y compare x)") =
    forAll { (x: Size, y: Size) =>
      Claim((x compare y) == (-1 * (y compare x)))
    }

  property("(x < y) != (x >= y)") =
    forAll { (x: Size, y: Size) =>
      Claim((x < y) != (x >= y))
    }

  property("(x <= y) != (x > y)") =
    forAll { (x: Size, y: Size) =>
      Claim((x <= y) != (x > y))
    }

  property("(x == y) = (x <= y && x >= y)") =
    forAll { (x: Size, y: Size) =>
      Claim((x == y) == ((x <= y) && (x >= y)))
    }

  property("(x == y) == (x.toString == y.toString)") =
    forAll { (x: Size, y: Size) =>
      Claim((x == y) == (s"$x" == s"$y"))
    }

  property("(x == y) == (x.approxString == y.approxString)") =
    forAll { (x: Size, y: Size) =>
      Claim((x == y) == (x.approxString == y.approxString))
    }

  property("isFinite") =
    forAll { (x: Size) =>
      Claim(x.isFinite == (x != Size.Unbounded))
    }

  property("min") =
    forAll { (x: Size, y: Size) =>
      val lhs = (x min y) == x
      val rhs = (x compare y) <= 0
      Claim(lhs == rhs)
    }

  property("max") =
    forAll { (x: Size, y: Size) =>
      val lhs = (x max y) == x
      val rhs = (x compare y) >= 0
      Claim(lhs == rhs)
    }

  property("from BigInt") =
    forAll { (x0: BigInt, y0: BigInt) =>
      val (x, y) = (x0.abs, y0.abs)
      Claim((Size(x) + Size(y)) == Size(x + y))
    }
}
