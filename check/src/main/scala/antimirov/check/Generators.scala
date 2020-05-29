package antimirov.check

import antimirov.Rx
import org.scalacheck.Gen

object Generators {

  def string(rx: Rx): Gen[String] =
    rx match {
      case v @ Rx.Var(_) =>
        sys.error(s"illegal regex detected ($v)")
      case Rx.Phi =>
        sys.error(s"impossible to generate strings from regex ϕ")
      case Rx.Empty =>
        Gen.const("")
      case Rx.Letter(c) =>
        Gen.const(s"$c")
      case Rx.Letters(cs) =>
        Gen.choose(0, cs.size - 1).map(cs.get(_).toString)
      case c @ Rx.Concat(_, _) =>
        val gs = c.concats.map(string)
        Gen.tailRecM((gs, "")) {
          case (Nil, s) => Gen.const(Right(s))
          case (g :: gs, s) => g.map(s => Left((gs, s)))
        }
      case c @ Rx.Choice(_, _) =>
        val gs = c.concats.map(string)
        Gen.oneOf(gs).flatMap(identity)
      case Rx.Star(r) =>
        lazy val g: Gen[String] =
          Gen.oneOf(Gen.const(""), string(r).flatMap(s1 => g.map(s2 => s1 + s2)))
        g
      case Rx.Repeat(r, m, n) =>
        val g = string(r)
        Gen.choose(m, n).flatMap(i => Gen.listOfN(i, g).map(_.mkString))
    }
}
