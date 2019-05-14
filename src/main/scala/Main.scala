package regsym

object Main {
  def main(args: Array[String]): Unit = {
    val a = Regex.Literal(Sym.A)
    val b = Regex.Literal(Sym.B)
    // val r = a * ((a * b).star + (b * a.star)) * b
    // val nfae = r.toNfaE
    // val dfa = nfae.toDfa
    // println(nfae.render)
    // println(dfa.render)
    // println(r.render)
    // println("")
    // println(dfa.toRegex.render)
    // println("")

    val r1 = a * (a * b).star
    println("\nr1:")
    println(r1.render)
    println(r1.toDfa.render)

    val r2 = a.star
    println("\nr2:")
    println(r2.render)
    println(r2.toDfa.render)

    val r3 = r1 & r2
    println("\nr3:")
    println(r3.toDfa.render)
    println(r3.render)

    println("\nr4:")
    //println((~r3.toDfa).render)
    val r4 = ~r3
    println("")
    println(r4.render)
  }
}
