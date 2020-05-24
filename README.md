## Antimirov

### Dedication

This project is named after Valentin Antimirov (1961 - 1995). His work
on [partial derivatives](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.56.2509)
of regular expressions is fundmental to this project.

### Overview

Antimirov is a Scala package for working with
[regular expressions](https://en.wikipedia.org/wiki/Regular_expression).

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
val z5: Rx = ~x     // [:-\uffff].*|[1-9][0-9]*[:-\uffff].*|[1-9][0-9]*[\u0000-/].*|[\u0000-0].*|

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

An `antimirov.Rx` value can be converted to `java.util.regex.Pattern`
or `scala.util.matching.Regex`.

Note that unlike many modern regex libraries, Antimirov's regular
expressions do not contain non-regular features (such as
back-references, zero-width assertions, and so on), and are solely
focused on matching, not on searching or extraction.

Concretely, this means that:

    1. Patterns are matched against the entire string
    2. No subgroup extraction is possible
    3. The only primitive operations are alternation, concatenation, and Kleene star

In exchange for giving up these modern affordances, Antimirov can do
things that most regular expression libraries can't, such as
intersection, exclusive-or, negation, semantic equality checks, set
comparisons (e.g. inclusion), and more.

### Getting Antimirov

Antimirov supports Scala 2.13. If you use SBT, you can
include Antimirov via the following `build.sbt` snippet:

```
libraryDependencies += "org.spire-math" %% "antimirov" % "0.0.1"
```

Antimirov also supports Scala.js. To use Antimirov with Scala.js, use
the following `build.sbt` snippet:

```
libraryDependencies += "org.spire-math" %%% "antimirov" % "0.0.1"
```

### Details

TBD

### Known Issues

    1. Antimirov doesn't preserve user-specified expression syntax
    2. Ranges are sometimes split unnecessarily
    3. Negative ranges (e.g. [^a-z]) are not generated internally
    4. Antimirov has no ability to simplify/canonicalize expressions
    5. Antimirov cannot (yet) check constant expressions at compile-time
    6. Repetition (e.g. {3,5}) is not yet supported
    7. Synthetic operators such as + and ? are not reified in the AST
    8. There are probably still efficiency gains

### Future Work

TBD

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2020.
