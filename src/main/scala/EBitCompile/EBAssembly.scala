package HomNAND

case class EBAssembly(inst: List[EBAInst], global: Map[String, List[Short]]) {
  def bootStrap: EBAssembly = {
    val bootMain = List(
      AisConst(IRShort(0x7fff.toShort)),
      DisA,
      RisD(SPReg),
      AisConst(IRLabel("main_ret")),
      DisA,
      RefRisD(SPReg),
      AisConst(IRLabel("main")),
      IRJMP,
      IRLabelStmt("main_ret"),
      DisR(RtReg),
      RisD(MainReg),
      AisConst(IRShort(1.toShort)),
      DisA,
      RisD(FinishReg),
      AisConst(IRLabel("finish_loop")),
      IRLabelStmt("finish_loop"),
      IRJMP
    ).map(_.toAsm).foldLeft(List[EBAInst]())(_ ++ _)
    EBAssembly(bootMain ++ inst, global)
  }
  def toBin: (List[Short], List[Short]) = {
    val (sOffset, ramc) = global.toList.foldLeft(Map[String, Int](), List[Short]()) {
      case ((s, r), (ns, nr)) => ((s + (ns -> (0x40 + r.length))), r ++ nr)
    }
    val (rOffset, romi) = inst.foldLeft(sOffset, List[EBARInst]()) {
      case ((s, r), i: EBARInst) => (s, r :+ i)
      case ((s, r), LInst(v)) => ((s + (v -> r.length)), r)
    }
    val romc = romi.map((i) => i.toBin(rOffset))
    (romc.padTo(32768, 0.toShort), (List.fill(0x40)(0.toShort) ++ ramc).padTo(32768, 0.toShort))
  }
}

sealed trait EBAInst
sealed trait EBARInst extends EBAInst {
  def toBin(omap: Map[String, Int]): Short
}
case class AInst(a: IValue) extends EBARInst {
  def toBin(omap: Map[String, Int]) = a match {
    case IConstV(v) => (v & 0x7fff).toShort
    case ILabelV(s) => (omap(s) & 0x7fff).toShort
  }
}
case class CInst(d: IDest, c: IOp, j: IJump) extends EBARInst {
  def toBin(omap: Map[String, Int]) = (0xe000 |
    ((d.toBin & 0x7) << 3) |
    ((c.toBin & 0x7f) << 6) |
    (j.toBin & 0x7)).toShort
}
case class LInst(s: String) extends EBAInst

sealed trait IValue
case class IConstV(v: Short) extends IValue
case class ILabelV(s: String) extends IValue

sealed trait IDest {
  def toBin: Int
}
case object IDNull extends IDest {
  def toBin = 0
}
case object IDM extends IDest {
  def toBin = 1
}
case object IDD extends IDest {
  def toBin = 2
}
case object IDMD extends IDest {
  def toBin = 3
}
case object IDA extends IDest {
  def toBin = 4
}
case object IDAM extends IDest {
  def toBin = 5
}
case object IDAD extends IDest {
  def toBin = 6
}
case object IDAMD extends IDest {
  def toBin = 7
}

sealed trait IOp {
  def toBin: Int
}
case object IOZero extends IOp {
  def toBin = 0x2a
}
case object IOOne extends IOp {
  def toBin = 0x3f
}
case object IONegone extends IOp {
  def toBin = 0x3a
}
case object IOD extends IOp {
  def toBin = 0x0c
}
case object IOA extends IOp {
  def toBin = 0x30
}
case object IOM extends IOp {
  def toBin = 0x70
}
case object IONegD extends IOp {
  def toBin = 0xd
}
case object IONegA extends IOp {
  def toBin = 0x31
}
case object IONegM extends IOp {
  def toBin = 0x71
}
case object IOMinD extends IOp {
  def toBin = 0xf
}
case object IOMinA extends IOp {
  def toBin = 0x33
}
case object IOMinM extends IOp {
  def toBin = 0x73
}
case object IODAddOne extends IOp {
  def toBin = 0x1f
}
case object IOAAddOne extends IOp {
  def toBin = 0x37
}
case object IOMAddOne extends IOp {
  def toBin = 0x77
}
case object IODSubOne extends IOp {
  def toBin = 0xe
}
case object IOASubOne extends IOp {
  def toBin = 0x32
}
case object IOMSubOne extends IOp {
  def toBin = 0x72
}
case object IODAddA extends IOp {
  def toBin = 0x2
}
case object IODAddM extends IOp {
  def toBin = 0x42
}
case object IODSubA extends IOp {
  def toBin = 0x13
}
case object IODSubM extends IOp {
  def toBin = 0x53
}
case object IOASubD extends IOp {
  def toBin = 0x7
}
case object IOMSubD extends IOp {
  def toBin = 0x47
}
case object IODAndA extends IOp {
  def toBin = 0x0
}
case object IODAndM extends IOp {
  def toBin = 0x40
}
case object IODOrA extends IOp {
  def toBin = 0x15
}
case object IODOrM extends IOp {
  def toBin = 0x55
}

sealed trait IJump {
  def toBin: Int
}
case object IJNull extends IJump {
  def toBin = 0x0
}
case object IJGT extends IJump {
  def toBin = 0x1
}
case object IJEQ extends IJump {
  def toBin = 0x2
}
case object IJGE extends IJump {
  def toBin = 0x3
}
case object IJLT extends IJump {
  def toBin = 0x4
}
case object IJNE extends IJump {
  def toBin = 0x5
}
case object IJLE extends IJump {
  def toBin = 0x6
}
case object IJMP extends IJump {
  def toBin = 0x7
}
