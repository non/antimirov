package regsym

object Dfa {

  class Builder[A](starting: Set[A], ending: A) {
    private var assignments: Map[Set[A], Int] = Map.empty
    private var nextId: Int = 1
    var dfa: Dfa = Dfa.alloc(assign(starting))
    var seen: Set[Set[A]] = Set.empty

    def assign(st: Set[A]): Int =
      assignments.getOrElse(st, {
        val id = nextId
        assignments = assignments.updated(st, id)
        nextId += 1
        id
      })

    def markSeen(st: Set[A]): Unit =
      seen += st

    def isEnd(st: Set[A]): Boolean =
      st(ending)

    def addEdge(st0: Set[A], sym: Sym, st1: Set[A]): Unit = {
      val (n0, n1) = (assign(st0), assign(st1))
      dfa = dfa.addEdge(n0, sym, n1)
      if (isEnd(st0)) dfa = dfa.addAccept(n0)
      if (isEnd(st1)) dfa = dfa.addAccept(n1)
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

  def addEdge(from: Int, c: Sym, to: Int): Dfa =
    Dfa(start, accept, edges.updated(from, edges.get(from) match {
      case None => Map(c -> to)
      case Some(m) => m.updated(c, to)
    }))

  def addAccept(st: Int): Dfa =
    if (accept(st)) this
    else Dfa(start, accept + st, edges.updated(st, Map.empty))

  def follow(from: Int, c: Sym): Option[Int] =
    edges.get(from).flatMap(_.get(c))

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
}
