package regsym

import scala.annotation.tailrec

sealed abstract class Regex extends Product with Serializable {

  def render: String =
    this match {
      case Regex.Impossible => "∅"
      case Regex.Empty => "ε"
      case Regex.Literal(sym) => sym.toString
      case Regex.Or(x, y) => s"(${x.render}|${y.render})"
      case Regex.Then(x, y) => s"${x.render}${y.render}"
      case Regex.Star(x) => s"(${x.render})*"
    }

  override def toString: String = render

  def *:(sym: Sym): Regex =
    Regex.Literal(sym) * this

  def *(that: Regex): Regex =
    if (this == Regex.Impossible) this
    else if (that == Regex.Impossible) that
    else if (this == Regex.Empty) that
    else if (that == Regex.Empty) this
    else Regex.Then(this, that)

  def +(that: Regex): Regex =
    if (this == Regex.Impossible) that
    else if (that == Regex.Impossible) this
    else Regex.Or(this, that)

  def equiv(r: Regex): Boolean =
    subsetOf(r) && supersetOf(r)

  def subsetOf(r: Regex): Boolean =
    (this & (~r)) == Regex.Impossible

  def supersetOf(r: Regex): Boolean =
    ((~this) & r) == Regex.Impossible

  def unary_~ : Regex =
    (~this.toDfa).toRegex

  def &(that: Regex): Regex =
    (this.toDfa & that.toDfa).toRegex

  def star: Regex =
    this match {
      case Regex.Impossible => Regex.Empty
      case Regex.Empty => Regex.Empty
      case r @ Regex.Star(_) => r
      case _ => Regex.Star(this)
    }

  def toDfa: Dfa =
    toNfa.toDfa

  def toNfa: Nfa = {
    def recur(r: Regex, namer: Namer): Nfa =
      r match {
        case Regex.Impossible =>
          Nfa.alloc(namer(), namer())
        case Regex.Empty =>
          val n = namer()
          Nfa.alloc(n, n)
        case Regex.Literal(s) =>
          val (start, accept) = (namer(), namer())
          Nfa.alloc(start, accept)
            .addEdge(start, Some(s), accept)
        case Regex.Then(r1, r2) =>
          val nfa1 = recur(r1, namer)
          val nfa2 = recur(r2, namer)
          Nfa.alloc(nfa1.start, nfa2.accept)
            .absorb(nfa1)
            .absorb(nfa2)
            .addEdge(nfa1.accept, None, nfa2.start)
        case Regex.Or(r1, r2) =>
          val start = namer()
          val nfa1 = recur(r1, namer)
          val nfa2 = recur(r2, namer)
          val accept = namer()
          Nfa.alloc(start, accept)
            .absorb(nfa1)
            .absorb(nfa2)
            .addEdge(start, None, nfa1.start)
            .addEdge(start, None, nfa2.start)
            .addEdge(nfa1.accept, None, accept)
            .addEdge(nfa2.accept, None, accept)
        case Regex.Star(r) =>
          val start = namer()
          val nfa = recur(r, namer)
          val accept = namer()
          Nfa.alloc(start, accept)
            .absorb(nfa)
            .addEdge(start, None, accept)
            .addEdge(start, None, nfa.start)
            .addEdge(nfa.accept, None, start)
      }
    recur(this, new Namer())
  }
}

object Regex {
  case object Impossible extends Regex
  case object Empty extends Regex
  case class Literal(s: Sym) extends Regex
  case class Then(r1: Regex, r2: Regex) extends Regex
  case class Or(r1: Regex, r2: Regex) extends Regex
  case class Star(r: Regex) extends Regex
}
