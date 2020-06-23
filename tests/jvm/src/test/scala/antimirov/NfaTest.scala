package antimirov

import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Util._

object NfaTest extends Properties("NfaTest") { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(500)
      //.withPropFilter(Some("regression"))

  property("nfa accepts regex strings") =
    Prop.forAll(genRxAndStrs) { case (rx, set) =>
      val nfa = rx.toNfa
      set.iterator.map { s =>
        val lhs = rx.accepts(s)
        val rhs = nfa.accepts(s)
        Claim(lhs == rhs) :| s"failed to accept '$s'"
      }.foldLeft(Prop(true))(_ && _)
    }

  property("builer toString") =
    Prop.forAll(genRx) { rx =>
      Claim(Nfa.Builder.fromRx(rx).toString != "")
    }

  property("accepts = !rejects") =
    Prop.forAll(genRxAndStrs) { case (rx, set) =>
      val nfa = rx.toNfa
      set.iterator.map { s =>
        Claim(nfa.rejects(s) != nfa.accepts(s))
      }.foldLeft(Prop(true))(_ && _)
    }

  property("(x = y) -> (x.toString = y.toString)") =
    Prop.forAll(genRx, genRx) { (rx1, rx2) =>
      val (nfa1, nfa2) = (rx1.toNfa, rx2.toNfa)
      Claim((nfa1 != nfa2) || (s"$nfa1" == s"$nfa2"))
    }

  property("nfa regression #1") =
    Claim(Rx.parse("[ab]b").toNfa.rejects("aa"))
}
