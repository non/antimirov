package regsym

sealed abstract class Sym extends Product with Serializable

object Sym {

  val All: List[Sym] = List(A, B)

  case object A extends Sym
  case object B extends Sym
}
