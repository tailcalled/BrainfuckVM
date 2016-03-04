package bfvm

case class BFAst(body: Vector[BFInstr]) {
  override def toString = body.mkString("")
  def ++(that: BFAst) = BFAst(body ++ that.body)
}
sealed trait BFInstr
object BFInstr {
  case object Succ extends BFInstr {
    override def toString = "+"
  }
  case object Pred extends BFInstr {
    override def toString = "-"
  }
  case object Next extends BFInstr {
    override def toString = ">"
  }
  case object Prev extends BFInstr {
    override def toString = "<"
  }
  case object Outp extends BFInstr {
    override def toString = "."
  }
  case object Inpt extends BFInstr {
    override def toString = ","
  }
  case class  Loop(body: BFAst) extends BFInstr {
    override def toString = s"[$body]"
  }
}