package antimirov

import org.scalacheck.{Gen, Prop, Properties}
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}

object BitSetTest extends Properties("BitSetTest") {

  val genSize = Gen.choose(1, 120)

  def genElem(sz: Int): Gen[Int] =
    Gen.choose(0, sz - 1)

  def genBits(sz: Int): Gen[BitSet] =
    Gen.listOf(genElem(sz)).map(xs => BitSet(sz, xs))

  property("BitSet ~ Set[Int]") =
    forAll(genSize) { sz =>
      forAll(Gen.listOf(genElem(sz))) { xs =>
        val set = Set(xs: _*)
        val bitset = BitSet(sz, xs)
        (0 until sz)
          .map(x => Claim(set(x) == bitset(x)))
          .foldLeft(Claim(set.size == bitset.size))(_ && _)
      }
    }

  property("(x.maxSize != y.maxSize) => (x != y)") =
    forAll(genSize, genSize) { (sz1, sz2) =>
      forAll(genBits(sz1), genBits(sz2)) { (x, y) =>
        if (x.maxSize != y.maxSize) Claim(x != y) else Claim(true)
      }
    }

  property("(x == y) -> (x.hashCode = y.hashCode)") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz)) { (x, y) =>
        if (x == y) Claim(x.hashCode == y.hashCode)
        else Claim(true)
      }
    }

  property("(x | y) = (y | x)") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz)) { (x, y) =>
        Claim((x | y) == (y | x))
      }
    }

  property("(x | y)(n) = x(n) || y(n)") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz), Gen.choose(0, sz - 1)) { (x, y, n) =>
        Claim((x | y)(n) == (x(n) || y(n)))
      }
    }

  property("((x | y) | z) = (x | (y | z))") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz), genBits(sz)) { (x, y, z) =>
        Claim(((x | y) | z) == (x | (y | z)))
      }
    }

  property("(x & y) = (y & x)") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz)) { (x, y) =>
        Claim((x & y) == (y & x))
      }
    }

  property("((x & y) & z) = (x & (y & z))") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz), genBits(sz)) { (x, y, z) =>
        Claim(((x & y) & z) == (x & (y & z)))
      }
    }

  property("(x intersects y) ~ (x & y)") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genBits(sz)) { (x, y) =>
        Claim((x intersects y) == ((x & y) != BitSet.empty(sz)))
      }
    }

  property("x += n") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genElem(sz)) { (x, n) =>
        x -= n
        x += n
        Claim(x(n))
      }
    }

  property("x -= n") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genElem(sz)) { (x, n) =>
        x += n
        x -= n
        Claim(!x(n))
      }
    }

  property("x.clear()") =
    forAll(genSize) { sz =>
      forAll(genBits(sz)) { x =>
        x.clear()
        Claim(x == BitSet.empty(sz))
      }
    }

  property("x.copy()") =
    forAll(genSize) { sz =>
      forAll(genBits(sz), genElem(sz)) { (x, n) =>
        x -= n
        val y = x.copy()
        x += n
        Claim(x != y)
      }
    }
}
