package bfvm

class BFVM(code: BFProg, val ram: Array[Byte] = new Array[Byte](300000), _out: Int => Unit = x => {
  System.out.write(x); System.out.flush()
}, _in: () => Int = () => {
  System.in.read()
}) {
  
  var mc = 0
  var ic = 0
  
  val instrs = {
    import BFAsm._
    code.instrs.map {
      case Mod(b) => (0x00 << 24) | (b & 0xFF)
      case Mov(s) => (0x01 << 24) | (s & 0xFFFF)
      case Out => 0x02 << 24
      case Inp => 0x03 << 24
      case IfZ => 0x04 << 24
      case Jmp(pos) =>
        if (pos < 0) throw new Exception("Jump to negative instruction address")
        Integer.MIN_VALUE + pos
    }.toArray
  }
  
  def step() = {
    if (ic < instrs.length) {
      val i = instrs(ic)
      (i & 0xFF000000) match {
        case 0x00000000 => ram(mc) = (ram(mc) + (i & 0xFF)).toByte
        case 0x01000000 => mc += (i & 0xFFFF).toShort
        case 0x02000000 => _out(ram(mc))
        case 0x03000000 =>
          val ch = _in()
          if (ch == -1) ram(mc) = 0
          else ram(mc) = ch.toByte
        case 0x04000000 =>
          if (ram(mc) != 0) ic += 1
        case x if x < 0 =>
          val pos = 0x7FFFFFFF & i
          ic = pos - 1
      }
      ic += 1
      true
    }
    else {
      false
    }
  }
  
}