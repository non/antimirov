package demo

import antimirov.Rx
import antimirov.props.Regex
import scalaprops.{Gen, Property, Scalaprops}

object Demo extends Scalaprops {

  val r1 = Regex("-?(0|[1-9][0-9]*)")
  val implicitStyle =
    Property.forAll { (w: r1.Word) =>
      val s: String = w.value
      // s is guaranteed to be accepted by r1
      true
    }

  val r2 = Rx.parse("-?(0|[1-9][0-9]*)")
  val mygen: Gen[String] = Regex.gen(r2)
  val explicitStyle =
    Property.forAllG(mygen) { s =>
      // s is guaranteed to be accepted by r2
      true
    }
}
