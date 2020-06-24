package antimirov

sealed abstract class Edge {

  def get(c: Char): Option[Int]
  def toLetterMap: LetterMap[Int]
  def iterator: Iterator[((Char, Char), Int)] = toLetterMap.iterator

  def mapValues(f: Int => Int): Edge =
    this match {
      case Edge.NoEdge => Edge.NoEdge
      case Edge.SingleEdge(c, n) => Edge.SingleEdge(c, f(n))
      case Edge.AnyEdge(n) => Edge.AnyEdge(f(n))
      case Edge.AsciiEdge(arr, lm) => Edge.AsciiEdge(arr.map(f), lm.mapValues(f))
      case Edge.MapEdge(m, lm) => Edge.MapEdge(m.map { case (k, v) => (k, f(v)) }, lm.mapValues(f))
      case Edge.LetterMapEdge(lm) => Edge.LetterMapEdge(lm.mapValues(f))
    }
}

object Edge {

  def apply(lm: LetterMap[Int]): Edge =
    if (lm.isEmpty) {
      NoEdge
    } else if (lm.size == 1) {
      val ((c, _), n) = lm.iterator.next
      SingleEdge(c, n)
    } else if (lm.maxKeyOption.get < 256) {
      val array = Array.fill(256)(-1)
      lm.items.foreach { case (c, n) => array(c.toInt) = n }
      AsciiEdge(array, lm)
    } else if (lm.size <= 64) {
      MapEdge(lm.toMap, lm)
    } else {
      lm.iterator.next match {
        case (('\u0000', '\uffff'), n) => AnyEdge(n)
        case _ => LetterMapEdge(lm)
      }
    }

  case object NoEdge extends Edge {
    def get(c: Char): Option[Int] = None
    val toLetterMap = LetterMap.empty[Int]
  }

  case class SingleEdge(char: Char, dest: Int) extends Edge {
    val result = Some(dest)
    def get(c: Char): Option[Int] = if (c == char) result else None
    val toLetterMap = LetterMap(char, dest)
  }

  case class AnyEdge(dest: Int) extends Edge {
    val result = Some(dest)
    def get(c: Char): Option[Int] = result
    val toLetterMap = LetterMap(LetterSet.Full, dest)
  }

  case class AsciiEdge(array: Array[Int], toLetterMap: LetterMap[Int]) extends Edge {
    def get(c: Char): Option[Int] = {
      if (c >= 256) return None
      val x = array(c.toInt)
      if (x >= 0) return Some(x) else None
    }
  }

  case class MapEdge(m: Map[Char, Int], toLetterMap: LetterMap[Int]) extends Edge {
    def get(c: Char): Option[Int] = m.get(c)
  }

  case class LetterMapEdge(toLetterMap: LetterMap[Int]) extends Edge {
    def get(c: Char): Option[Int] = toLetterMap.get(c)
  }
}
