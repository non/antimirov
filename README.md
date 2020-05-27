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
 - Matching ranges of characters (`Rx.Letters(cs)`)
 - Alternation (`Rx.Choice(x, y)`, i.e. `x + y`)
 - Concatenation (`Rx.Concat(x, y)`, i.e. `x * y`)
 - Kleene Star (`Rx.Star(x)`, i.e. `x.star`)
 - Repetition (`Rx.Repeat(r, m, n)`, i.e. `r.repeat(m, n)`)

In addition to the previous combinators which are reified as algebraic
data types, `Rx` supports additional operations:

 - Exponentiation (`x.pow(k)`)
 - Intersection (`x & y`)
 - Exclusive-or (XOR, i.e. `x ^ y`)
 - Difference (`x - y`)
 - Complement (`~x`)
 - Equality (`x === y`)
 - Partial-ordering (`x < y`, `x partialCompare y`, etc.)
 - Derivatives (`x.deriv(c)`)

These operations are consistent with the corresponding set operations.
What this means is that each `Rx` value has a corresponding set of
strings it accepts, and that these operations produce new `Rx` values
whose sets are consistent with the corresponding set operations.

Finaly, `Rx` values can be compiled down to an `Nfa` value for more
efficient matching.

### Getting Antimirov

Antimirov supports Scala 2.13 and 2.12. It is not yet published.

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

An `antimirov.Rx` value can be converted to an `antimirov.Nfa`, a
`java.util.regex.Pattern`, or a `scala.util.matching.Regex`.

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

### Known Issues

The biggest issue with this library is that the problems are
exponential in the general case. This means there are plenty of
expressions for which Antimirov's operations (equality, inclusion,
intersection, and so on) are prohibitively slow.

Here's a list of other known problems:

    1. Antimirov doesn't preserve user-specified expression syntax
    2. Antimirov cannot (yet) check constant expressions at compile-time
    3. Synthetic operators such as + and ? are not reified in the AST
    4. There's very little support for minimization or simplification
    5. DFA support is not implemented yet

### Future Work

Since the general problem is exponential, there is likely a lot of
future work around chipping away at the margins: heuristics that cover
most interesting regular expressions users are interested in.

It would be interesting to see how many of Antimirov's features we can
preserve if we allow extracting subgroup matches.

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2020.
