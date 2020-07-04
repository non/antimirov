package demo

import antimirov.Rx
import antimirov.props.Regex
import scalaprops.{Gen, Property, Scalaprops}
import scala.util.{Failure, Success, Try}

object PropsTest extends Scalaprops {

  val r1 = Regex("-?(0|[1-9][0-9]*)")
  val implicitStyle =
    Property.forAll { (w: r1.Word) =>
      val s: String = w.value
      // s is guaranteed to be accepted by r1
      r1.accepts(s)
    }

  val r2 = Rx.parse("-?(0|[1-9][0-9]*)")
  val mygen: Gen[String] = Regex.gen(r2)
  val explicitStyle =
    Property.forAllG(mygen) { s =>
      // s is guaranteed to be accepted by r2
      !r2.rejects(s)
    }

  val validHashEq =
    Property.forAllG(Gen.asciiString) { (s: String) =>
      Try(Rx.parse(s)) match {
        case Failure(_) =>
          true
        case Success(rx) =>
          val (r1, r2) = (Regex(rx), Regex(rx))
          r1.hashCode == r2.hashCode && r1 == r2
      }
    }
}
