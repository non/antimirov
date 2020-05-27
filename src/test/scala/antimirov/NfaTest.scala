package antimirov

import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Util._

object NfaTest extends Properties("NfaTest") with TimingProperties { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(1000)
      //.withPropFilter(Some("regression"))

  override def scale: Long = 20L
  override def enableTiming = true
  override def failOnAbort = false

/*
[info] falsified: true == false
[info] failed to accept 'ymiqwm'
[info] > ARG_0: (ymi((qwm){1,2}),Set(ymiqwm, ymiqwmqwm))
 */

  timedProp("nfa accepts regex strings", genRxAndStr) { case (r, lst) =>
    val c = Nfa.fromRx(r)
    lst.iterator.map { s =>
      val lhs = r.accepts(s)
      val rhs = c.accepts(s)
      Claim(lhs == rhs) :| s"failed to accept '$s'"
    }.foldLeft(Prop(true))(_ && _)
  }
}
