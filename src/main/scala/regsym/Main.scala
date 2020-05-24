// package regsym
// 
// object Main {
//   def main(args: Array[String]): Unit = {
//     val a = Regex.Literal(Sym.A)
//     val b = Regex.Literal(Sym.B)
//     val r1 = a * (a * b).star
//     val r2 = a.star
//     val r3 = r1 & r2
//     val r4 = ~r3
//     val r5 = ~r4
// 
//     println(s"(${r1.render}) & (${r2.render}) -> ${r3.render}")
//     println(s"~(${r3.render}) -> ${r4.render}")
//     println(s"~(${r4.render}) -> ${r5.render}")
//   }
// }
