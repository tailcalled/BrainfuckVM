package bfvm

object BFCompiler {
  
  import BFInstr._
  import BFAsm._
  
  def compile(code: BFAst): BFProg = code.body.map {
    case Succ => BFProg(Mod(1.toByte))
    case Pred => BFProg(Mod(-1.toByte))
    case Next => BFProg(Mov(1.toShort))
    case Prev => BFProg(Mov(-1.toShort))
    case Outp => BFProg(Out)
    case Inpt => BFProg(Inp)
    case Loop(body) => BFProg.loop(compile(body))
  }.foldLeft(BFProg())(_ ++ _)
  
}