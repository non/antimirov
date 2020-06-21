package antimirov

import org.scalacheck.{Arbitrary, Prop, Properties}
import org.typelevel.claimant.Claim
import scala.util.Try

import Prop.{forAllNoShrink => forAll}

object LazyStreamTest extends Properties("LazyStreamTest") {

  def stream[A](xs: List[A]): LazyStream[A] =
    LazyStream.fromIterable(xs)

  implicit def arbitraryStream[A: Arbitrary]: Arbitrary[LazyStream[A]] =
    Arbitrary(Arbitrary.arbitrary[List[A]].map(stream))

  property("fromIterable(xs).toList = xs") =
    forAll { (xs: List[Int]) =>
      Claim(stream(xs).toList == xs)
    }

  property("map") =
    forAll { (xs: List[Int], f: Int => String) =>
      Claim(stream(xs).map(f).toList == xs.map(f))
    }

  property("flatMap") =
    forAll { (xs: List[Int], f: Int => List[Byte]) =>
      Claim(stream(xs).flatMap(x => stream(f(x))).toList == xs.flatMap(f))
    }

  property("flatMap ~ map") =
    forAll { (xs: List[Int], f: Int => String) =>
      val ys = stream(xs)
      val lhs = ys.flatMap(y => LazyStream(f(y)))
      val rhs = ys.map(f).toList
      Claim(lhs.toList == rhs.toList)
    }

  property("isEmpty = !nonEmpty") =
    forAll { (xs: LazyStream[Int]) =>
      Claim(xs.isEmpty == !xs.nonEmpty)
    }

  property("concat works") =
    forAll { (xs: List[Int], ys: List[Int]) =>
      val lhs = LazyStream.concat(stream(xs), stream(ys)).toList
      val rhs = xs ++ ys
      Claim(lhs == rhs)
    }

  def boom[A]: LazyStream[A] =
    sys.error("boom!")

  property("concat is lazy") =
    forAll { (xs: LazyStream[Int]) =>
      val _ = LazyStream.concat(xs, boom)
      Prop(true)
    }

  property("defer is lazy") = {
    val _ = LazyStream.defer(boom)
    Prop(true)
  }

  property("empty iterator throws") = {
    val xs = LazyStream.empty[Int]
    val t = Try(xs.iterator.next)
    Claim(t.isFailure)
  }
}
