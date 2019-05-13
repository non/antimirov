package regsym

import scala.annotation.tailrec

sealed abstract class Regex extends Product with Serializable {

  def *(that: Regex): Regex = Regex.Then(this, that)
  def +(that: Regex): Regex = Regex.Or(this, that)
  def star: Regex = Regex.Star(this)

  def toNfaE: NfaE = {
    def recur(r: Regex, namer: Namer): NfaE =
      r match {
        case Regex.Impossible =>
          NfaE.alloc(namer(), namer())
        case Regex.Empty =>
          val n = namer()
          NfaE.alloc(n, n)
        case Regex.Literal(s) =>
          val (start, accept) = (namer(), namer())
          NfaE.alloc(start, accept)
            .addEdge(start, Some(s), accept)
        case Regex.Then(r1, r2) =>
          val nfa1 = recur(r1, namer)
          val nfa2 = recur(r2, namer)
          NfaE.alloc(nfa1.start, nfa2.accept)
            .absorb(nfa1)
            .absorb(nfa2)
            .addEdge(nfa1.accept, None, nfa2.start)
        case Regex.Or(r1, r2) =>
          val start = namer()
          val nfa1 = recur(r1, namer)
          val nfa2 = recur(r2, namer)
          val accept = namer()
          NfaE.alloc(start, accept)
            .absorb(nfa1)
            .absorb(nfa2)
            .addEdge(start, None, nfa1.start)
            .addEdge(start, None, nfa2.start)
            .addEdge(nfa1.accept, None, accept)
            .addEdge(nfa2.accept, None, accept)
        case Regex.Star(r) =>
          val nfa = recur(r, namer)
          nfa
            .addEdge(nfa.start, None, nfa.accept)
            .addEdge(nfa.accept, None, nfa.start)
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
