package antimirov

import org.scalacheck.{Prop, Properties}

object ReadmeTest extends Properties("ReadmeTest") {

  property("compiles") = {
    import antimirov.Rx

    val x: Rx = Rx.parse("[1-9][0-9]*")

    require(x.accepts("0") == false)
    require(x.accepts("1")== true)
    require(x.accepts("19")== true)
    require(x.accepts("09") == false)

    val y: Rx = Rx.parse("[0-9a-f][0-9a-f]")

    require(y.accepts("af")== true)
    require(y.accepts("09")== true)
    require(y.accepts("099") == false)

    // set operations
    //
    // note that the full Char range is:
    //   ['\u0000', ..., '/', '0', ... '9', ':', ... '\uffff']

    require((x | y) === Rx.parse("[1-9][0-9]*|[0-9a-f][0-9a-f]"))
    require((x & y) === Rx.parse("[1-9][0-9]"))
    require((x ^ y) === Rx.parse("0[0-9a-f]|[1-9][0-9][0-9][0-9]*|[1-9][a-f]|[1-9]|[a-f][0-9a-f]"))
    require((x - y) === Rx.parse("[1-9][0-9][0-9][0-9]*|[1-9]"))
    require((~x) === Rx.parse("[:-\uffff].*|[1-9][0-9]*[:-\uffff].*|[1-9][0-9]*[\u0000-/].*|[\u0000-0].*|"))

    // equality, subset, and superset comparisons

    val xx: Rx = Rx.parse("[1-4][0-9]*|[5-9][0-9]*")
    require((x == xx) == false)
    require((x === xx) == true)
    require((x <= xx) == true)
    require((x < xx) == false)

    val U: Rx = Rx.parse(".*")
    require((x == U) == false)
    require((x === U) == false)
    require((x <= U) == true)
    require((x < U) == true)

    Prop(true)
  }
}
