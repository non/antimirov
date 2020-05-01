package regsym

import scala.collection.mutable
import scala.util.Random

object Dfa {

  class Builder[P](starting: P)(isEnd: P => Boolean) {
    private var assignments: Map[P, Int] = Map.empty
    private var nextId: Int = 1
    var dfa: Dfa = Dfa.alloc(assign(starting))
    var seen: Set[P] = Set.empty

    if (isEnd(starting)) dfa = dfa.addAccept(assign(starting))

    def assign(st: P): Int =
      assignments.getOrElse(st, {
        val id = nextId
        assignments = assignments.updated(st, id)
        nextId += 1
        id
      })

    def markSeen(st: P): Unit =
      seen += st

    def addEdge(st0: P, sym: Sym, st1: P): Unit = {
      val (n0, n1) = (assign(st0), assign(st1))
      this.dfa = this.dfa.addEdge(n0, sym, n1)
      if (isEnd(st0)) this.dfa = this.dfa.addAccept(n0)
      if (isEnd(st1)) this.dfa = this.dfa.addAccept(n1)
    }
  }

  def alloc(start: Int): Dfa =
    Dfa(start, Set.empty, Map(start -> Map.empty))
}

case class Dfa(
  start: Int,
  accept: Set[Int],
  edges: Map[Int, Map[Sym, Int]]
) {

  def accepts(s: List[Sym]): Boolean = {
    def loop(st0: Int, s: List[Sym]): Boolean =
      s match {
        case Nil =>
          accept(st0)
        case sym :: rest =>
          edges.get(st0) match {
            case None =>
              false
            case Some(m) =>
              m.get(sym) match {
                case None => false
                case Some(st1) => loop(st1, rest)
              }
          }
      }
    loop(start, s)
  }

  def addEdge(from: Int, c: Sym, to: Int): Dfa =
    Dfa(start, accept, edges.updated(from, edges.get(from) match {
      case None => Map(c -> to)
      case Some(m) => m.updated(c, to)
    }))

  def addAccept(st: Int): Dfa =
    if (accept(st)) this
    else Dfa(start, accept + st, edges.get(st) match {
      case None => edges.updated(st, Map.empty)
      case Some(m) => edges
    })

  def follow(st: Int, sym: Sym): Option[Int] =
    edges.get(st).flatMap(m => m.get(sym))

  def unary_~ : Dfa = {
    val bldr: Dfa.Builder[Int] = new Dfa.Builder(start)(n => !accept(n))
    val dead = Int.MinValue
    def loop(queue: List[Int]): Dfa =
      queue match {
        case Nil =>
          bldr.dfa
        case p0 :: rest =>
          if (bldr.seen(p0)) {
            loop(rest)
          } else {
            var q = rest
            Sym.All.foreach { sym =>
              val p1 = this.follow(p0, sym).getOrElse(dead)
              bldr.addEdge(p0, sym, p1)
              q = p1 :: q
            }
            bldr.markSeen(p0)
            loop(q)
          }
      }
    loop(start :: Nil)
  }

  def &(that: Dfa): Dfa = {
    val start = (this.start, that.start)
    val bldr = new Dfa.Builder(start)({ case (a1, a2) =>
      this.accept(a1) && that.accept(a2)
    })
    def loop(queue: List[(Int, Int)]): Dfa =
      queue match {
        case Nil =>
          bldr.dfa
        case (p0 @ (x0, y0)) :: rest =>
          var q: List[(Int, Int)] = rest
          if (bldr.seen(p0)) {
            loop(rest)
          } else {
            Sym.All.foreach { sym =>
              (this.follow(x0, sym), that.follow(y0, sym)) match {
                case (Some(x1), Some(y1)) =>
                  val p1 = (x1, y1)
                  bldr.addEdge(p0, sym, p1)
                  q = p1 :: q
                case _ => ()
              }
            }
            bldr.markSeen(p0)
            loop(q)
          }
      }
    loop(start :: Nil)
  }

  def render: String = {
    val sb = new StringBuilder()
    edges.keys.toList.sorted.foreach { k =>
      if (k == start && accept(k)) {
        sb.append(s"starting and accepting state $k:\n")
      } else if (k == start) {
        sb.append(s"starting state $k:\n")
      } else if (accept(k)) {
        sb.append(s"accepting state $k:\n")
      } else {
        sb.append(s"state $k:\n")
      }
      edges(k).foreach { case (sym, dest) =>
        sb.append(s"  $sym -> $dest\n")
      }
    }
    sb.toString
  }

  def toRegex: Regex = {
    val b = mutable.Map.empty[Int, Regex]
    def bget(i: Int): Regex = b.getOrElse(i, Regex.Impossible)
    def bset(i: Int, r: Regex): Unit =
      r match {
        case Regex.Impossible => ()
        case _ => b(i) = r
      }

    val a = mutable.Map.empty[(Int, Int), Regex]
    def aget(i: Int, j: Int): Regex = a.getOrElse((i, j), Regex.Impossible)
    def aset(i: Int, j: Int, r: Regex): Unit =
      r match {
        case Regex.Impossible => ()
        case _ => a((i, j)) = r
      }

    edges.foreach { case (i, m) =>
      if (accept(i)) {
        bset(i, Regex.Empty)
      }
      m.foreach { case (sym, j) =>
        aset(i, j, aget(i, j) + Regex.Literal(sym))
      }
    }

    val states = edges.keys.toList.sorted

    def solve(sts: List[Int]): Regex =
      sts match {
        case Nil =>
          bget(states.head)
        case n :: ns =>
          val ann = aget(n, n).star
          bset(n, ann * bget(n))

          ns.foreach { j =>
            aset(n, j, ann * aget(n, j))
          }

          ns.foreach { i =>
            bset(i, bget(i) + (aget(i, n) * bget(n)))
            ns.foreach { j =>
              aset(i, j, aget(i, j) + (aget(i, n) * aget(n, j)))
            }
          }

          solve(ns)
      }
    solve(states.reverse)
  }
}
