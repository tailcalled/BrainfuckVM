package bfvm

import java.io.StringReader

object HelloWorld {
  
  val source = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  
  def main(args: Array[String]) = {
    val ast = BFParser.parse(new StringReader(source))
    println(ast)
    val asm = BFCompiler.compile(ast)
    println(asm)
    val vm = new BFVM(asm)
    val t0 = System.nanoTime()
    while (vm.step()) {}
    val t1 = System.nanoTime()
    println()
    println(s"Done in ${t1 - t0} ns")
  }
  
}