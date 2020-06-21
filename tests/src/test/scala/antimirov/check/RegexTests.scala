package antimirov.check

import antimirov.Rx
import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}

object RegexTest extends Properties("RegexTest") {

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)

  property("Regex(s) = Regex(Rx.parse(s))") = {
    val s = "[abc]*d"
    val x = Regex(s)
    val y = Regex(Rx.parse(s))
    Claim((x == y) && (x.hashCode == y.hashCode) && (s"$x" == s"$y"))
  }

  val r1 = Regex("(a|b)c{2,4}d*e")

  property("Arbitrary[r1.Word]") =
    forAll { (w: r1.Word) =>
      Claim(r1.accepts(w.value))
    }

  property("r1.gen") =
    forAll(r1.gen) { s =>
      Claim(!r1.rejects(s))
    }

  val r2 = Regex("""-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\-+]?[0-9]+)?""")

  property("Arbitrary[r2.Word]") =
    forAll { (w: r2.Word) =>
      Claim(!r2.rejects(w.value))
    }

  property("r2.gen") =
    forAll(r2.gen) { s =>
      Claim(r2.accepts(s))
    }
}
