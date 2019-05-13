package regsym

object Main {
  def main(args: Array[String]): Unit = {
    val a = Regex.Literal(Sym.A)
    val b = Regex.Literal(Sym.B)
    val r = a * ((a * b).star + (b * a.star)) * b
    val nfae = r.toNfaE
    val dfa = nfae.toDfa
    println(nfae.render)
    println(dfa.render)
  }
}
