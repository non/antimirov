[![Build Status](https://api.travis-ci.org/non/antimirov.svg)](https://travis-ci.org/non/antimirov)
[![codecov.io](http://codecov.io/github/non/antimirov/coverage.svg?branch=master)](http://codecov.io/github/non/antimirov?branch=master)

## Antimirov

### Dedication

This project is named after Valentin Antimirov (1961 - 1995). His work
on [partial derivatives](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.2509)
of regular expressions is fundmental to this project.

### Overview

Antimirov is a Scala package for working with
[regular expressions](https://en.wikipedia.org/wiki/Regular_expression).

Antimirov defines an `Rx` type, which supports the following regular
expression combinators:

 - Unmatchable regular expressions (`Rx.Phi`)
 - Matching the empty string (`Rx.Empty`)
 - Matching single characters (`Rx.Letter(c)`)
 - Matching sets of characters (`Rx.Letters(cs)`)
 - Alternation (`Rx.Choice(x, y)`, i.e. `x + y` or `x | y`)
 - Concatenation (`Rx.Concat(x, y)`, i.e. `x * y`)
 - Kleene Star (`Rx.Star(x)`, i.e. `x.star`)
 - Repetition (`Rx.Repeat(r, m, n)`, i.e. `r.repeat(m, n)`)

In addition to the previous combinators which are reified as algebraic
data types, `Rx` supports additional operations:

 - Exponentiation (`x.pow(k)`)
 - Optionality (`x.optional`)
 - Intersection (`x & y`)
 - Exclusive-or (XOR, i.e. `x ^ y`)
 - Difference (`x - y`)
 - Complement (`~x`)
 - Equality (`x === y`)
 - Partial-ordering (`x < y`, `x partialCompare y`, etc.)
 - Derivatives (`x.deriv(c)`, `x.partialDeriv(c)`)

These operations are consistent with the corresponding set operations.
What this means is that each `Rx` value has a corresponding set of
strings it accepts, and that these operations produce new `Rx` values
whose sets are consistent with the corresponding set operations.

Finaly, `Rx` values can be compiled down to an automaton for more
efficient matching (either a `Dfa` or `Nfa`).

### Getting Antimirov

Antimirov supports Scala 2.13 and 2.12. It is avilable from Sonatype.

To include Antimirov in your projects, you can use the following
`build.sbt` snippet:

```scala
libraryDependencies +=
  "org.spire-math" %% "antimirov-core" % "0.2.4"

libraryDependencies += // optional scalacheck support
  "org.spire-math" %% "antimirov-check" % "0.2.4"

libraryDependencies += // optional scalaprops support
  "org.spire-math" %% "antimirov-props" % "0.2.4"
```

Antimirov also supports Scala.js. To use Antimirov in your Scala.js
projects, include the following `build.sbt` snippet:

```scala
libraryDependencies +=
  "org.spire-math" %%% "antimirov-core" % "0.2.4"

libraryDependencies += // optional scalacheck support
  "org.spire-math" %%% "antimirov-check" % "0.2.4"

libraryDependencies += // optional scalaprops support
  "org.spire-math" %%% "antimirov-props" % "0.2.4"
```

### Details

Antimirov provides an algebraic interface for building regular
expressions, as well as testing them for equality, subset/superset
relationships, and more:

```scala
import antimirov.Rx

val x: Rx = Rx.parse("[1-9][0-9]*")

x.accepts("0")      // false
x.accepts("1")      // true
x.accepts("19")     // true
x.accepts("09")     // false

val y: Rx = Rx.parse("[0-9a-f][0-9a-f]")

y.accepts("af")     // true
y.accepts("09")     // true
y.accepts("099")    // false

// set operations
//
// note that the full Char range is:
//   ['\u0000', ..., '/', '0', ... '9', ':', ... '\uffff']

val z1: Rx = x | y  // [1-9][0-9]*|[0-9a-f][0-9a-f]
val z2: Rx = x & y  // [1-9][0-9]
val z3: Rx = x ^ y  // 0[0-9a-f]|[1-9][0-9][0-9][0-9]*|[1-9][a-f]|[1-9]|[a-f][0-9a-f]
val z4: Rx = x - y  // [1-9][0-9][0-9][0-9]*|[1-9]
val z5: Rx = ~x     // [^1-9].*|[1-9][0-9]*[^0-9].*|

// equality, subset, and superset comparisons

val xx: Rx = Rx.parse("[1-4][0-9]*|[5-9][0-9]*")
x == xx  // false
x === xx // true
x <= xx  // true
x < xx   // false

val U: Rx = Rx.parse(".*")
x == U   // false
x === U  // false
x <= U   // true
x < U    // true
```

An `antimirov.Rx` value can be converted to any of:

 - `antimirov.Nfa`
 - `antimirov.Dfa`
 - `java.util.regex.Pattern`
 - `scala.util.matching.Regex`.

Note that unlike many modern regex libraries, Antimirov's regular
expressions do not contain non-regular features (such as
back-references), and are solely focused on matching, not on searching
or extraction.

Concretely, this means that:

 1. Patterns are matched against the entire string
 2. No subgroup extraction is possible
 3. Zero-width assertions are unsupported

Technically zero-width assertions are still regular, but they are not
easily composable. For example, for `(?=x)` as a zero-width look-ahead
assertion on `x`, and `(?<=y)` as a zero-width look-behind assertion
on `y`, the following is true:

 - `[a-z]*(?=[^b])` is equivalent to `[a-z]*[^b]`
 - `(?<=[^c])[a-z]*` is equivalent to `[^c][a-z]*`
 - `[a-z]*(?=[^b])(?<=[^c])[a-z]*)` is *NOT* equivalent to `[a-z]*[^b][^c][a-z]*`
 - instead it's equivalent to `[a-z]*[^bc][a-z]*`

These assertions don't play well with the algebraic structure of `Rx`
which is why they are left out (for now at least).

In exchange for giving up these sorts of affordances, Antimirov can do
things that most regular expression libraries can't do, such as
intersection, exclusive-or, negation, semantic equality checks, subset
comparisons (e.g. inclusion), and more.

### ScalaCheck support

The `antimirov-check` package support ScalaCheck, adding the ability
to generate strings according to a regular expression. There are two
ways to use it:

```scala
package demo

import antimirov.Rx
import antimirov.check.Regex
import org.scalacheck.{Prop, Properties}

object Demo extends Properties("Demo") {

  property("Arbitrary-based usage") = {
    val r1 = Regex("-?(0|[1-9][0-9]*)")
    Prop.forAll { (w: r1.Word) =>
      val s: String = w.value
      // s is guaranteed to be accepted by r1
      ???
    }
  }

  property("Gen-based usage") = {
    val r2 = Rx.parse("-?(0|[1-9][0-9]*)")
    val mygen: Gen[String] = Regex.gen(r2)
    Prop.forAll(mygen) { s: String =>
      // s is guaranteed to be accepted by r2
      ???
    }
  }
}
```

One thing to note here is that `antimirov.Rx` has no direct dependency
on ScalaCheck, which is why we introduce `antimirov.check.Regex` to
gain `Arbitrary` support.

`Regex` is wrapper type around `Rx` that adds the path-dependent type
`Word` as well as implementations of `Gen` and `Arbitrary` used by
ScalaCheck. `Regex` does not support the full suit of Antimirov
operations (such as `&`) and is really just meant for use with
ScalaCheck. (For other usages, prefer its embedded `Rx` value).

### ScalaProps support

Antimirov also supports ScalaProps via `antimirov-props`. This package
is very similar to `antimirov-check`:

```scala
package demo

import antimirov.Rx
import antimirov.props.Regex
import scalaprops.{Gen, Property, Scalaprops}

object Demo extends Scalaprops {

  val implicitStyle = {
    val r1 = Regex("-?(0|[1-9][0-9]*)")
    Property.forAll { (w: r1.Word) =>
      val s: String = w.value
      // s is guaranteed to be accepted by r1
      ???
    }
  }

  val explicitStyle = {
    val r2 = Rx.parse("-?(0|[1-9][0-9]*)")
    val mygen: Gen[String] = Regex.gen(r2)
    Property.forAllG(mygen) { s =>
      // s is guaranteed to be accepted by r2
      ???
    }
  }
}
```

### Web/JS tool

Antimirov has an interactive JS/HTML tool for working with regular
expressions.

You can try it out at
[http://phylactery.org/antimirov/](http://phylactery.org/antimirov/).

To visit this page locally, simply run `sbt web/fullOptJS` and then
visit `web/index.html` in your web browser.

### Known Issues

The biggest issue with this library is that the problems are
exponential in the general case. This means there are plenty of
expressions for which Antimirov's operations (equality, inclusion,
intersection, and so on) are prohibitively slow.

There are some good strategies for dealing with this complexity
through heuristics and optimizations. But some constructions (such as
very wide alternations contained within a Kleene star) will probably
never perform very well.

Here's a list of other known problems:

  1. Antimirov doesn't preserve user-specified expression syntax
  2. Antimirov cannot (yet) check constant expressions at compile-time
  3. Synthetic operators (e.g. `+` and `?`) are not reified in the AST
  4. There could be more work on simplification/canonicalization

### Future Work

Since the general problem is exponential, there is likely a lot of
future work around chipping away at the margins: heuristics that cover
most interesting regular expressions users are interested in.

We could add support for other kinds of generators and test frameworks
(e.g. Hedgehog, Scalaprops, etc.)

We could add predefined character classes (e.g. `\w` from PCRE or
`[:digit:]` from POSIX). It's fairly straightforward to add support
for reading these, but a bit tricker figuring out when to emit them.

There is room for improvement of the automata implementations
(particularly `antimirov.NFA`). Relatedly, we could add support for
searching instead of just matching.

There is major room for improvement for the HTML/JS tool (the current
version was optimized for what was easy for the author to produce).

Additionally, we could provide an interactive/command-line tool to
help work with regular expressions (potentially using Graal or Scala
Native to produce a native executable).

It would be interesting to see how many of Antimirov's features we
would have to give up to add support for extracting subgroup matches.

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2020.
