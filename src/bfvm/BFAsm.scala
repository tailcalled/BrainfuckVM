package bfvm

import BFAsm._

case class BFProg(instrs: Vector[BFAsm]) {
  def length = instrs.length
  def ++(that: BFProg) = BFProg(instrs ++ that.instrs.map {
    case Jmp(to) => Jmp(to + instrs.length)
    case x => x
  })
  override def toString() = {
    val digits = (instrs.length - 1).toString.length
    instrs.zipWithIndex.map { case (instr, ix) => s"${"0"*(digits - ix.toString.length)}$ix $instr"}.mkString("\n")
  }
}
object BFProg {
  def apply(instrs: BFAsm*): BFProg = BFProg(Vector(instrs:_*))
  def loop(prog: BFProg) = BFProg(IfZ, Jmp(3 + prog.length)) ++ prog ++ BFProg(Jmp(-2 - prog.length))
}
sealed trait BFAsm
object BFAsm {
  case class  Mod(amount: Byte) extends BFAsm {
    override def toString() = s"MOD ${if (amount > 0) "+" else ""}$amount"
  }
  case class  Mov(amount: Short) extends BFAsm {
    override def toString() = s"MOV ${if (amount > 0) "+" else ""}$amount"
  }
  case object Out extends BFAsm {
    override def toString() = "OUT"
  }
  case object Inp extends BFAsm {
    override def toString() = "IN"
  }
  case object IfZ extends BFAsm {
    override def toString() = "IFZ"
  }
  case class  Jmp(to: Int)extends BFAsm {
    override def toString = s"JMP $to"
  }
}