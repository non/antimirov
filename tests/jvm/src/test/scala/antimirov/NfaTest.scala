package antimirov

import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Util._

object NfaTest extends Properties("NfaTest") with TimingProperties { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("regression"))

  override def scale: Long = 20L
  override def enableTiming = true
  override def failOnAbort = false

  timedProp("nfa accepts regex strings", genRxAndStrs) { case (rx, set) =>
    val nfa = rx.toNfa
    set.iterator.map { s =>
      val lhs = rx.accepts(s)
      val rhs = nfa.accepts(s)
      Claim(lhs == rhs) :| s"failed to accept '$s'"
    }.foldLeft(Prop(true))(_ && _)
  }

  timedProp("accepts = !rejects", genRxAndStrs) { case (rx, set) =>
    val nfa = rx.toNfa
    set.iterator.map { s =>
      Claim(nfa.rejects(s) != nfa.accepts(s))
    }.foldLeft(Prop(true))(_ && _)
  }

  timedProp("(x = y) -> (x.toString = y.toString)", genRx, genRx) { (rx1, rx2) =>
    val (nfa1, nfa2) = (rx1.toNfa, rx2.toNfa)
    Claim((nfa1 != nfa2) || (s"$nfa1" == s"$nfa2"))
  }

  property("nfa regression #1") =
    Claim(Rx.parse("[ab]b").toNfa.rejects("aa"))
}
