package HomNAND

case class EBIRProg(predef: Map[String, List[Short]], fundef: Map[String, EBIRFun]) {
  def toAsm : EBAssembly = {
    val fd = fundef.toList.map{
      case (i, EBIRFun(j)) => j.map(_.toAsm).foldLeft(List[EBAInst]())(_ ++ _)
    }.foldLeft(List[EBAInst]())(_ ++ _)
    EBAssembly(fd, predef)
  }
}

case class EBIRFun(body: List[EBIRStmt])

sealed trait EBIRStmt {
  def toAsm: List[EBAInst]
}
case class IRLabelStmt(s: String) extends EBIRStmt {
  override def toString = s"$s :"
  def toAsm = List(LInst(s))
}
case class AisConst(v: IRConst) extends EBIRStmt {
  override def toString = s"A = $v"
  def toAsm = List(AInst(v.toAsm))
}
case object AisD extends EBIRStmt {
  override def toString = "A = D"
  def toAsm = List(CInst(IDA, IOD, IJNull))
}
case class AisDOpConst(op: IROp, v: IRConst) extends EBIRStmt {
  override def toString = s"A = D $op $v"
  def toAsm = {
    val nOp = op match {
      case IRAdd => IODAddA
      case IRSub => IODSubA
      case IRAnd => IODAndA
      case IROr => IODOrA
    }
    List(AInst(v.toAsm), CInst(IDA, nOp, IJNull))
  }
}
case object DisAref extends EBIRStmt {
  override def toString = "D = [A]"
  def toAsm = {
    List(CInst(IDD, IOM, IJNull))
  }
}
case object DisA extends EBIRStmt {
  override def toString = "D = A"
  def toAsm = {
    List(CInst(IDD, IOA, IJNull))
  }
}
case class DisR(r: IRIReg) extends EBIRStmt {
  override def toString = s"D = $r"
  def toAsm = {
    List(AInst(r.absLoc), CInst(IDD, IOM, IJNull))
  }
}
case class RisDOpR(d: IRReg, op: IROp, r: IRIReg) extends EBIRStmt {
  override def toString = s"$d = D $op $r"
  def toAsm = {
    val nOp = op match {
      case IRAdd => IODAddM
      case IRSub => IODSubM
      case IRAnd => IODAndM
      case IROr => IODOrM
    }
    d match {
      case DReg => List(AInst(r.absLoc), CInst(IDD, nOp, IJNull))
      case w: IRIReg => List(
        AInst(r.absLoc),
        CInst(IDD, nOp, IJNull),
        AInst(w.absLoc),
        CInst(IDM, IOD, IJNull))
    }

  }
}
case class RisD(d: IRReg) extends EBIRStmt {
  override def toString = s"$d = D"
  def toAsm = {
    d match {
      case DReg => List()
      case w: IRIReg => List(
        AInst(w.absLoc),
        CInst(IDM, IOD, IJNull))
    }
  }
}
case class DisDOp(op: IRSOp) extends EBIRStmt {
  override def toString = s"D = $op D"
  def toAsm = {
    val nOp = op match {
      case IRNeg => IONegD
      case IRMin => IOMinD
    }
    List(CInst(IDD, nOp, IJNull))
  }
}
case class RisROpConst(d: IRReg, op: IROp, r: IRIReg, c: IRConst) extends EBIRStmt {
  override def toString = s"$d = $r $op $c"
  def toAsm = {
    val nOp = op match {
      case IRAdd => IODAddM
      case IRSub => IOMSubD
      case IRAnd => IODAndM
      case IROr => IODOrM
    }
    d match {
      case DReg => List(AInst(c.toAsm), CInst(IDD, IOA, IJNull), AInst(r.absLoc), CInst(IDD, nOp, IJNull))
      case w: IRIReg => List(AInst(c.toAsm), CInst(IDD, IOA, IJNull), AInst(r.absLoc), CInst(IDD, nOp, IJNull),
        AInst(w.absLoc), CInst(IDM, IOD, IJNull))
    }
  }
}
case class RefRisD(d: IRIReg) extends EBIRStmt {
  override def toString = s"[$d] = D"
  def toAsm = {
    List(AInst(d.absLoc), CInst(IDA, IOM, IJNull), CInst(IDM, IOD, IJNull))
  }
}
case class RefConstisD(c: IRConst) extends EBIRStmt {
  override def toString = s"[$c] = D"
  def toAsm = {
    List(AInst(c.toAsm), CInst(IDM, IOD, IJNull))
  }
}
case object IRJMP extends EBIRStmt {
  override def toString = s"JMP"
  def toAsm = { List(CInst(IDNull, IOD, IJMP)) }
}
case object IRJEQ extends EBIRStmt {
  override def toString = s"JEQ"
  def toAsm = { List(CInst(IDNull, IOD, IJEQ)) }
}
case object IRJNE extends EBIRStmt {
  override def toString = s"JNE"
  def toAsm = { List(CInst(IDNull, IOD, IJNE)) }
}
case object IRJLT extends EBIRStmt {
  override def toString = s"JLT"
  def toAsm = { List(CInst(IDNull, IOD, IJLT)) }
}
case object IRJGT extends EBIRStmt {
  override def toString = s"JGT"
  def toAsm = { List(CInst(IDNull, IOD, IJGT)) }
}
case object IRJLE extends EBIRStmt {
  override def toString = s"JLE"
  def toAsm = { List(CInst(IDNull, IOD, IJLE)) }
}
case object IRJGE extends EBIRStmt {
  override def toString = s"JGE"
  def toAsm = { List(CInst(IDNull, IOD, IJGE)) }
}

sealed trait IRReg

sealed trait IRIReg extends IRReg {
  def absLoc: IConstV
}
case class RReg(i: Int) extends IRIReg {
  def absLoc: IConstV = IConstV((i + 16).toShort)
}
case object SPReg extends IRIReg {
  def absLoc: IConstV = IConstV(15.toShort)
}
case object BPReg extends IRIReg {
  def absLoc: IConstV = IConstV(14.toShort)
}
case object RtReg extends IRIReg {
  def absLoc: IConstV = IConstV(13.toShort)
}
case object Rt2Reg extends IRIReg {
  def absLoc: IConstV = IConstV(12.toShort)
}
case object MainReg extends IRIReg {
  def absLoc: IConstV = IConstV(8.toShort)
}

case object DReg extends IRReg

sealed trait IRConst {
  def toAsm: IValue
}
case class IRShort(i: Short) extends IRConst {
  def toAsm = IConstV(i)
}
case class IRLabel(v: String) extends IRConst {
  def toAsm = ILabelV(v)
}
sealed trait IROp

case object IRAdd extends IROp
case object IRSub extends IROp
case object IRAnd extends IROp
case object IROr extends IROp

sealed trait IRSOp
case object IRNeg extends IRSOp
case object IRMin extends IRSOp
