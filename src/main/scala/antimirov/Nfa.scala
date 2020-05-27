package antimirov

object Nfa {
  def alloc(start: Int, end: Int): Nfa =
    Nfa(start, end, Map(start -> Map.empty, end -> Map.empty))

  def fromRx(rx: Rx): Nfa = {
    def recur(r: Rx, n: Int): (Nfa, Int) =
      r match {
        case Rx.Phi =>
          (Nfa.alloc(n, n + 1), n + 2)
        case Rx.Empty =>
          (Nfa.alloc(n, n), n + 1)
        case Rx.Letter(c) =>
          (Nfa.alloc(n, n + 1)
            .addEdge(n, Some(LetterSet(c)), n + 1), n + 2)
        case Rx.Letters(cs) =>
          (Nfa.alloc(n, n + 1)
            .addEdge(n, Some(cs), n + 1), n + 2)
        case Rx.Concat(r1, r2) =>
          val (nfa1, n1) = recur(r1, n)
          val (nfa2, n2) = recur(r2, n1)
          (Nfa.alloc(nfa1.start, nfa2.accept)
            .absorb(nfa1)
            .absorb(nfa2)
            .addEdge(nfa1.accept, None, nfa2.start), n2)
        case Rx.Choice(r1, r2) =>
          val start = n
          val (nfa1, n1) = recur(r1, n + 1)
          val (nfa2, n2) = recur(r2, n1)
          val accept = n2
          (Nfa.alloc(start, accept)
            .absorb(nfa1)
            .absorb(nfa2)
            .addEdge(start, None, nfa1.start)
            .addEdge(start, None, nfa2.start)
            .addEdge(nfa1.accept, None, accept)
            .addEdge(nfa2.accept, None, accept), n2 + 1)
        case Rx.Star(r) =>
          val start = n
          val (nfa, n1) = recur(r, n + 1)
          val accept = n1
          (Nfa.alloc(start, accept)
            .absorb(nfa)
            .addEdge(start, None, accept)
            .addEdge(start, None, nfa.start)
            .addEdge(nfa.accept, None, start), n1 + 1)
        case Rx.Repeat(r, x, y) if x > 0 =>
          recur(Rx.Concat(r, Rx.Repeat(r, x - 1, y - 1)), n)
        case Rx.Repeat(r, _, y) if y > 0 =>
          recur(Rx.Choice(Rx.Empty, Rx.Concat(r, Rx.Repeat(r, 0, y - 1))), n)
        case Rx.Repeat(r, _, _) =>
          recur(Rx.Empty, n)
        case v @ Rx.Var(_) =>
          sys.error(s"illegal var node found: $v")
      }
    recur(rx, 0)._1
  }
}

case class Nfa(
  start: Int,
  accept: Int,
  edges: Map[Int, Map[Option[LetterSet], Set[Int]]]
) {

  def renumber(n: Int): Nfa =
    Nfa(
      start + n,
      accept + n,
      edges.map { case (x, m) =>
        (x + n, m.map { case (o, set) => (o, set.map(_ + n)) })
      })

  def addEdge(from: Int, c: Option[LetterSet], to: Int): Nfa =
    Nfa(start, accept, edges.get(from) match {
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

  def closureFollow(from: Set[Int], sym: LetterSet): Set[Int] =
    from.flatMap(n => closure(follow(n, Some(sym))))

  def follow(from: Int, c: Option[LetterSet]): Set[Int] =
    edges.get(from).flatMap(_.get(c)).getOrElse(Set.empty)

  def closure(from: Set[Int]): Set[Int] = {
    def loop(s0: Set[Int]): Set[Int] = {
      val s1 = s0 | s0.flatMap(s => follow(s, None))
      if (s1 == s0) s0 else loop(s1)
    }
    loop(from)
  }

  def transitions: Iterator[(Int, Option[LetterSet], Int)] =
    for {
      pair0 <- edges.iterator
      (from, m) = pair0
      pair1 <- m.iterator
      (c, set) = pair1
      to <- set.iterator
    } yield (from, c, to)

  def absorb(nfa: Nfa): Nfa =
    nfa.transitions.foldLeft(this) {
      case (nfa, (from, c, to)) => nfa.addEdge(from, c, to)
    }

  def render: String = {
    val sb = new StringBuilder
    edges.keys.toList.sorted.foreach { k =>
      sb.append((k == start, k == accept) match {
        case (true, true) => s"starting and accepting state $k:\n"
        case (true, false) => s"starting state $k:\n"
        case (false, true) => s"accepting state $k:\n"
        case (false, false) => s"state $k:\n"
      })
      edges(k).foreach { case (sym, dests) =>
        val e = sym.map(_.toString).getOrElse("ε")
        sb.append(s"  $e -> ${dests.mkString(", ")}\n")
      }
    }
    sb.toString
  }
}

case class CompressedNfa(
  size: Int,
  start: BitSet,
  accept: BitSet,
  edges: LetterMap[Array[BitSet]]) {

  override def toString: String = {
    val e = edges.mapValues {
      case null => "null"
      case arr => arr.iterator.map {
        case null => "null"
        case bs => bs.toString
      }.mkString("Array(", ", ", ")")
    }
    s"CompressedNfa($size, $start, $accept, $e)"
  }

  def accepts(s: String): Boolean = {
    var i = 0
    var st = start
    while (i < s.length) {
      follow(st, s.charAt(i)) match {
        case Some(st1) => st = st1
        case None => return false
      }
      i += 1
    }
    st intersects accept
  }

  def rejects(s: String): Boolean =
    !accepts(s)

  def follow(st0: BitSet, c: Char): Option[BitSet] =
    edges.get(c).map { array =>
      var i = 0
      val st1 = BitSet.empty(size)
      while (i < array.length) {
        if (st0(i)) {
          val bs = array(i)
          if (bs != null) st1 |= bs
        }
        i += 1
      }
      st1
    }
}

object CompressedNfa {
  def fromNfa(nfa: Nfa): CompressedNfa = {
    val size: Int = nfa.edges.size
    val start = BitSet(size, nfa.closure(Set(nfa.start)))
    val accept = BitSet(size, List(nfa.accept))

    val it: Iterator[LetterMap[Array[BitSet]]] =
      (0 until size).iterator.map { idx =>
        nfa.edges(idx).iterator.map {
          case (Some(cs), set) if set.nonEmpty =>
            val arr = new Array[BitSet](size)
            val bs = BitSet(size, nfa.closure(set))
            arr(idx) = bs
            LetterMap(cs, arr)
          case _ =>
            LetterMap.empty[Array[BitSet]]
        }.foldLeft(LetterMap.empty[Array[BitSet]])(_ ++ _)
      }

    def f(acc: Array[BitSet], xs: Array[BitSet]): Array[BitSet] = {
      var i = 0
      while (i < xs.length) {
        val x = xs(i)
        if (x != null) {
          if (acc(i) == null) {
            acc(i) = BitSet.empty(size)
          }
          acc(i) |= x
        }
        i += 1
      }
      acc
    }

    val edges = it.foldLeft(LetterMap.empty[Array[BitSet]])((acc, xs) => acc.merge(xs)(f))

    CompressedNfa(size, start, accept, edges)
  }
}
