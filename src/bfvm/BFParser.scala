package bfvm

import java.io.Reader

object BFParser {
  
  import BFInstr._
  
  def parseInstr(ch: Char, input: Reader): Option[BFInstr] = ch match {
    case '+' => Some(Succ)
    case '-' => Some(Pred)
    case '>' => Some(Next)
    case '<' => Some(Prev)
    case '.' => Some(Outp)
    case ',' => Some(Inpt)
    case '[' => Some(Loop(parseLoopBody(input)))
    case _ => None
  }
  def parseLoopBody(input: Reader) = {
    var instrs = Vector[BFInstr]()
    var ch = '\0'
    while ({
      val i = input.read()
      if (i == -1) throw new Exception("Unexpected EOF")
      ch = i.toChar
      ch != ']'
    }) {
      parseInstr(ch, input).foreach(i => instrs :+= i)
    }
    BFAst(instrs)
  }
  def parse(input: Reader) = {
    var instrs = Vector[BFInstr]()
    var ch = '\0'
    while ({
      val i = input.read()
      if (i == -1) false
      else { ch = i.toChar; true}
    }) {
    	parseInstr(ch, input).foreach(i => instrs :+= i)
    }
    BFAst(instrs)
  }
  
}