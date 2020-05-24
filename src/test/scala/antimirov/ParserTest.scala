package antimirov

import org.scalacheck.{Prop, Properties}
import org.typelevel.claimant.Claim

import Prop.{forAllNoShrink => forAll}

import Util._

object ParserTest extends Properties("ParserTest") { self =>

  property("parsing preserves semantics") =
    forAll(genRx) { rx0 =>
      val s = rx0.repr
      val rx1 = Parser.parse(s)
      Claim(rx0 === rx1)
    }
}
