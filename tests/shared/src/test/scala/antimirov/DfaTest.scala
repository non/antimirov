package antimirov

import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Util._

object DfaTest extends Properties("DfaTest") { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("dfa regression #3"))

  property("dfa accepts regex strings") =
    Prop.forAll(genRxAndStrs) { case (rx, set) =>
      val dfa = rx.toDfa
      set.iterator.map { s =>
        val lhs = rx.accepts(s)
        val rhs = dfa.accepts(s)
        Claim(lhs == rhs) :| s"failed to agree on '$s'"
      }.foldLeft(Prop(true))(_ && _)
    }

  property("accepts = !rejects") =
    Prop.forAll(genRxAndStrs) { case (rx, set) =>
      val dfa = rx.toDfa
      set.iterator.map { s =>
        Claim(dfa.rejects(s) != dfa.accepts(s))
      }.foldLeft(Prop(true))(_ && _)
    }

  property("(x = y) -> (x.toString = y.toString)") =
    Prop.forAll(genRx, genRx) { (rx1, rx2) =>
      val (dfa1, dfa2) = (rx1.toDfa, rx2.toDfa)
      Claim((dfa1 != dfa2) || (s"$dfa1" == s"$dfa2"))
    }

  property("dfa regression #1") =
    Claim(Rx.parse("[ab]b").toDfa.rejects("aa"))

  property("dfa regression #2") =
    Claim(Rx.parse("").toDfa.accepts(""))

  property("minimization") =
    Prop.forAllNoShrink(genRxAndStrs) { case (rx, set) =>
      val dfa1 = rx.toDfa
      val dfa2 = dfa1.minimize
      set.iterator.map { s =>
        Prop(dfa1.accepts(s) == dfa2.accepts(s)) :| s"disagree on $s"
      }.foldLeft(Prop(true))(_ && _)
    }

  property("dfa regression #3") = {
    val rx = Rx.parse("(\\u8fd6.*)*")
    val dfa = rx.toDfa
    List("hi").iterator.map { s =>
      val lhs = rx.accepts(s)
      val rhs = dfa.accepts(s)
      Claim(lhs == rhs) :| s"failed to agree on '$s'"
    }.foldLeft(Prop(true))(_ && _)
  }
}
