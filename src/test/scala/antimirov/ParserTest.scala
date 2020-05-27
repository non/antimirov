package antimirov

import org.scalacheck.{Prop, Properties}
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}

import Util._

object ParserTest extends Properties("ParserTest") { self =>

  property("parsing preserves semantics") =
    forAll(genRx) { rx0 =>
      val rx1 = Parser.parse(rx0.reRepr)
      Claim(rx0 === rx1)
    }
}
