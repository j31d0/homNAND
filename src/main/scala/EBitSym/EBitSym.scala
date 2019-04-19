package HomNAND

sealed trait EBitSym {
  def xor(other: EBitSym): EBitSym
  def nand(o: EBitSym): EBitSym
  def reduce: EBitSym
  def eval(f: Vector[Boolean]): Boolean
  def toExpr: String
}
case class SymConstBit(v: Boolean) extends EBitSym {
  def xor(o: EBitSym) = if (v) SymNotBit(o).reduce else o
  def nand(o: EBitSym) = if (v) SymNotBit(o).reduce else o
  def reduce: EBitSym = this
  def eval(f: Vector[Boolean]) = v
  def toExpr = if (v) "true" else "false"
}
case class SymLocBit(v: Int) extends EBitSym {
  def xor(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymXorBit(this, o).reduce
  }
  def nand(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymNotBit(SymAndBit(this, o).reduce).reduce
  }
  def reduce: EBitSym = this
  def eval(f: Vector[Boolean]) = f(v)
  def toExpr = s"in[$v]"
}
case class SymNotBit(a: EBitSym) extends EBitSym {
  def xor(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymXorBit(this, o).reduce
  }
  def nand(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymNotBit(SymAndBit(this, o).reduce).reduce
  }
  def reduce: EBitSym = a match {
    case SymConstBit(true) => SymConstBit(false)
    case SymConstBit(false) => SymConstBit(true)
    case SymNotBit(f) => f
    case _ => this
  }
  def eval(f: Vector[Boolean]) = !a.eval(f)
  def toExpr = s"(!${a.toExpr})"
}
case class SymAndBit(a: EBitSym, b: EBitSym) extends EBitSym {
  def xor(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymXorBit(this, o).reduce
  }
  def nand(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymNotBit(SymAndBit(this, o).reduce).reduce
  }
  def reduce: EBitSym = (a, b) match {
    case (SymConstBit(true), SymConstBit(true)) => SymConstBit(true)
    case (SymConstBit(_), SymConstBit(_)) => SymConstBit(false)

    case (SymConstBit(true), _) => b
    case (SymConstBit(false), _) => SymConstBit(false)
    case (_, SymConstBit(true)) => a
    case (_, SymConstBit(false)) => SymConstBit(false)
    case _ => this
  }
  def eval(f: Vector[Boolean]) = a.eval(f) && b.eval(f)
  def toExpr = s"(${a.toExpr} && ${b.toExpr})"
}
case class SymOrBit(a: EBitSym, b: EBitSym) extends EBitSym {
  def xor(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymXorBit(this, o).reduce
  }
  def nand(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymNotBit(SymAndBit(this, o).reduce).reduce
  }
  def reduce: EBitSym = (a, b) match {
    case (SymConstBit(false), SymConstBit(false)) => SymConstBit(false)
    case (SymConstBit(_), SymConstBit(_)) => SymConstBit(true)
    case (SymConstBit(true), _) => SymConstBit(true)
    case (SymConstBit(false), _) => b
    case (_, SymConstBit(true)) => SymConstBit(true)
    case (_, SymConstBit(false)) => a
    case _ => this
  }
  def eval(f: Vector[Boolean]) = a.eval(f) || b.eval(f)
  def toExpr = s"(${a.toExpr} || ${b.toExpr})"
}
case class SymXorBit(a: EBitSym, b: EBitSym) extends EBitSym {
  def xor(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymXorBit(this, o).reduce
  }
  def nand(o: EBitSym) = o match {
    case SymConstBit(v) => if (v) SymNotBit(this).reduce else this
    case _ => SymNotBit(SymAndBit(this, o).reduce).reduce
  }
  def reduce: EBitSym = (a, b) match {
    case (SymConstBit(false), SymConstBit(true)) => SymConstBit(true)
    case (SymConstBit(true), SymConstBit(false)) => SymConstBit(true)
    case (SymConstBit(_), SymConstBit(_)) => SymConstBit(false)
    case (SymConstBit(true), _) => SymNotBit(b)
    case (SymConstBit(false), _) => b
    case (_, SymConstBit(true)) => SymNotBit(a)
    case (_, SymConstBit(false)) => a
    case _ => this
  }
  def eval(f: Vector[Boolean]) = a.eval(f) ^ b.eval(f)
  def toExpr = s"(${a.toExpr} ^ ${b.toExpr})"
}

class EFastCircuit {
  @native def fastnand(f: Array[Boolean]): Array[Boolean]
  @native def fastand(f: Array[Boolean]): Array[Boolean]
  @native def fastor(f: Array[Boolean]): Array[Boolean]
  @native def fastxor(f: Array[Boolean]): Array[Boolean]
  @native def fastnot(f: Array[Boolean]): Array[Boolean]
}
