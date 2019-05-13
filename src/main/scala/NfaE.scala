package regsym

object NfaE {
  def alloc(start: Int, end: Int): NfaE =
    NfaE(start, end, Map(start -> Map.empty, end -> Map.empty))
}

case class NfaE(
  start: Int,
  accept: Int,
  edges: Map[Int, Map[Option[Sym], Set[Int]]]
) {

  def addEdge(from: Int, c: Option[Sym], to: Int): NfaE =
    NfaE(start, accept, edges.get(from) match {
      case None =>
        edges.updated(from, Map(c -> Set(to)))
      case Some(m) =>
        m.get(c) match {
          case None =>
            edges.updated(from, m.updated(c, Set(to)))
          case Some(sts) =>
            edges.updated(from, m.updated(c, sts + to))
        }
    })

  def closureFollow(from: Set[Int], sym: Sym): Set[Int] =
    from.flatMap(n => closure(follow(n, Some(sym))))

  def follow(from: Int, c: Option[Sym]): Set[Int] =
    edges.get(from).flatMap(_.get(c)).getOrElse(Set.empty)

  def closure(from: Set[Int]): Set[Int] = {
    def loop(s0: Set[Int]): Set[Int] = {
      val s1 = s0 | s0.flatMap(s => follow(s, None))
      if (s1 == s0) s0 else loop(s1)
    }
    loop(from)
  }

  def absorb(nfa: NfaE): NfaE = {
    val triples = for {
      pair0 <- nfa.edges.iterator
      (from, m) = pair0
      pair1 <- m.iterator
      (c, set) = pair1
      to <- set.iterator
    } yield (from, c, to)
    triples.foldLeft(this: NfaE) {
      case (nfa, (from, c, to)) => nfa.addEdge(from, c, to)
    }
  }

  def toDfa: Dfa = {
    def loop(queue: List[Set[Int]], bldr: Dfa.Builder[Int]): Dfa = {
      //println(s"loop($queue, bldr")
      queue match {
        case Nil =>
          bldr.dfa
        case p0 :: ps =>
          if (bldr.seen(p0)) {
            loop(ps, bldr)
          } else {
            var q = ps
            val pa = closureFollow(p0, Sym.A)
            if (pa.nonEmpty) {
              bldr.addEdge(p0, Sym.A, pa)
              q = pa :: q
            }
            val pb = closureFollow(p0, Sym.B)
            if (pb.nonEmpty) {
              bldr.addEdge(p0, Sym.B, pb)
              q = pb :: q
            }
            bldr.markSeen(p0)
            loop(q, bldr)
          }
      }
    }

    val s = closure(Set(start))
    val bldr = new Dfa.Builder(s, accept)
    loop(s :: Nil, bldr)
  }

  def render: String = {
    val sb = new StringBuilder
    edges.keys.toList.sorted.foreach { k =>
      if (k == start && k == accept) {
        sb.append(s"starting and accepting state $k:\n")
      } else if (k == start) {
        sb.append(s"starting state $k:\n")
      } else if (k == accept) {
        sb.append(s"accepting state $k:\n")
      } else {
        sb.append(s"state $k:\n")
      }
      edges(k).foreach { case (sym, dests) =>
        val e = sym.getOrElse("ε")
        sb.append(s"  $e -> ${dests.mkString(", ")}\n")
      }
    }
    sb.toString
  }
}
