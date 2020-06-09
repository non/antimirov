package antimirov

import org.scalacheck.{Arbitrary, Gen, Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Arbitrary.arbitrary
import Util._

object NfaTest extends Properties("NfaTest") with TimingProperties { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("regression"))

  override def scale: Long = 20L
  override def enableTiming = true
  override def failOnAbort = false

  val genRxAndStrs: Gen[(Rx, Set[String])] =
    Gen.zip(genRxAndStr, arbitrary[Set[String]])
      .map { case ((rx, good), maybe) => (rx, good | maybe) }

  timedProp("nfa accepts regex strings", genRxAndStrs) { case (r, set) =>
    val c = Nfa.fromRx(r)
    set.iterator.map { s =>
      val lhs = r.accepts(s)
      val rhs = c.accepts(s)
      Claim(lhs == rhs) :| s"failed to accept '$s'"
    }.foldLeft(Prop(true))(_ && _)
  }

  timedProp("accepts = !rejects", genRxAndStrs) { case (r, set) =>
    val c = Nfa.fromRx(r)
    set.iterator.map { s =>
      Claim(c.rejects(s) != c.accepts(s))
    }.foldLeft(Prop(true))(_ && _)
  }

  timedProp("(x = y) -> (x.toString = y.toString)", genRx, genRx) { (r1, r2) =>
    val (nfa1, nfa2) = (Nfa.fromRx(r1), Nfa.fromRx(r2))
    Claim((nfa1 != nfa2) || (s"$nfa1" == s"$nfa2"))
  }

  property("nfa regression #1") =
    Claim(Rx.parse("[ab]b").toNfa.rejects("aa"))
}
