package antimirov

import org.scalacheck.{Prop, Properties, Test}
import org.typelevel.claimant.Claim

import Util._

object GraphvizTest extends Properties("GraphvizTest") { self =>

  override def overrideParameters(params: Test.Parameters): Test.Parameters =
    params
      .withMinSuccessfulTests(100)
      //.withPropFilter(Some("regression"))

  // quite basic for now, but at least we exercise it
  property("can draw dfa") =
    Prop.forAll(genRx) { rx =>
      val dfa = rx.toDfa
      val dot = Graphviz.dot(dfa)
      Claim(dot.startsWith("digraph g {"))
    }
}
