package HomNAND
import scala.io.Source

object EBCCompiler {
  var labelcnt = 0
  /*
  def apply(eprog: EBCProg): EBAssembly =  {

  }
  */
  def genLabel(): String = {
    val f = s"label_$labelcnt"
    labelcnt = labelcnt + 1
    f
  }

  def predefToIR(pdef: Map[String, (Option[BigInt], EBCType, Option[EBCInitValue])]): Map[String, List[Short]] = {
    pdef.map {
      case (k, (bop, _, iop)) => {
        val defLength = bop.getOrElse(BigInt(1)).toInt
        val outData = iop.map {
          case ConstStr(v) => v.map(_.toShort).toList :+ 0.toShort
          case ConstArr(v) => v.map(_.toShort)
          case ConstInt(v) => List(v.toShort)
        }.getOrElse(List()) padTo (defLength, 0.toShort)
        (k, outData)
      }
    }
  }

  def getFreshLoc(usedRegs: Int, usedTmp: Int, stackSize: Int): (TargetLoc, Int, Int) = {
    if (usedRegs > 15) (Mtarg(BPLoc(stackSize + usedTmp)), usedRegs, usedTmp + 1) else
      (Rtarg(RegLoc(usedRegs)), usedRegs + 1, usedTmp)

  }
  def stackSize(vars: List[(String, Option[BigInt])]): Int = {
    vars.foldLeft(0) {
      case (i, (_, fop)) => i + fop.map(_.toInt).getOrElse(1)
    }
  }
  def moveFromD(d: ExprLoc, intoD: List[EBIRStmt]): List[EBIRStmt] = d match {
    case DLoc => intoD
    case RegLoc(i) => intoD :+ RisD(RReg(i))
    case SPLoc(i) => intoD ++ List(
      RisD(Rt2Reg),
      if (i > 0) RisROpConst(RtReg, IRAdd, SPReg, IRShort(i.toShort))
      else RisROpConst(RtReg, IRSub, SPReg, IRShort((-i).toShort)),
      DisR(Rt2Reg),
      RefRisD(RtReg))
    case BPLoc(i) => intoD ++ List(
      RisD(Rt2Reg),
      if (i > 0) RisROpConst(RtReg, IRAdd, BPReg, IRShort(i.toShort))
      else RisROpConst(RtReg, IRSub, BPReg, IRShort((-i).toShort)),
      DisR(Rt2Reg),
      RefRisD(RtReg))
    case GlobalLoc(i) => intoD :+ RefConstisD(IRLabel(i))
  }

  def genOp(op: IROp, d: ExprLoc, e1: EBCExpr, e2: EBCExpr, usedRegs: Int,
    usedTmp: Int,
    globals: List[(String, Option[BigInt])],
    params: List[String],
    vars: List[(String, Option[BigInt])]): (List[EBIRStmt], Int, Int) = {
    val (e1d, ur, ut) = d match {
      case RegLoc(i) => (Rtarg(RegLoc(i)), usedRegs, usedTmp)
      case _ => getFreshLoc(usedRegs, usedTmp, stackSize(vars))
    }
    val (e1eval, mt, mp) = exprToIR(DLoc, e1, ur, ut, globals, params, vars)
    val (e2eval, mt2, mp2) = exprToIR(e1d.d, e2, usedRegs, usedTmp, globals, params, vars)
    val intoD = e1d match {
      case Rtarg(RegLoc(i)) => e2eval ++ e1eval :+ RisDOpR(DReg, op, RReg(i))
      case Mtarg(BPLoc(i)) => e2eval ++ e1eval ++ List(
        RisD(RtReg),
        DisR(BPReg),
        if (i > 0) AisDOpConst(IRAdd, IRShort(i.toShort))
        else AisDOpConst(IRSub, IRShort((-i).toShort)),
        DisAref,
        RisDOpR(DReg, IRAdd, RtReg))
    }
    (moveFromD(d, intoD), mt max mt2, mp max mp2)
  }

  def exprToIR(
    d: ExprLoc,
    expr: EBCExpr,
    usedRegs: Int,
    usedTmp: Int,
    globals: List[(String, Option[BigInt])],
    params: List[String],
    vars: List[(String, Option[BigInt])]): (List[EBIRStmt], Int, Int) = expr match {
    case EVarExp(v) => {
      val vi = vars indexWhere { case (x, y) => x == v }
      val pi = params indexWhere { case x => x == v }
      val gi = globals indexWhere { case (x, y) => x == v }
      val intoD = (vi, pi, gi) match {
        case (i, _, _) if i >= 0 => {
          val stackidx = (vars take i).foldLeft(0) {
            case (i, (_, None)) => i + 1
            case (i, (_, Some(j))) => (i + j).toInt
          }
          vars(i) match {
            case (_, None) => List(
              RisROpConst(DReg, IRSub, BPReg, IRShort(((stackidx + 1)).toShort)),
              AisD,
              DisAref)
            case (_, Some(s)) => List(
              RisROpConst(DReg, IRSub, BPReg, IRShort(((stackidx + s.toInt)).toShort)))
          }
        }
        case (_, i, _) if i >= 0 => List(
          RisROpConst(DReg, IRAdd, BPReg, IRShort((i + 2).toShort)),
          AisD,
          DisAref)
        case (_, _, i) if i >= 0 => {
          globals(i) match {
            case (s, None) => List(
              AisConst(IRLabel(s)),
              DisAref)
            case (s, Some(_)) => List(
              AisConst(IRLabel(s)),
              DisA)
          }
        }
        case (_, _, _) => throw new Exception("var not found")
      }
      (moveFromD(d, intoD), usedTmp, 0)
    }
    case EConstExp(ConstInt(i)) => {
      (moveFromD(d, List(
        AisConst(IRShort(i.toShort)),
        DisA)), usedTmp, 0)
    }
    case EDeref(EVarExp(v)) => {
      val vi = vars indexWhere { case (x, y) => x == v }
      val pi = params indexWhere { case x => x == v }
      val gi = globals indexWhere { case (x, y) => x == v }
      val intoD = (vi, pi, gi) match {
        case (i, _, _) if i >= 0 => {
          val stackidx = (vars take i).foldLeft(0) {
            case (i, (_, None)) => i + 1
            case (i, (_, Some(j))) => (i + j).toInt
          }
          vars(i) match {
            case (_, None) => List(
              RisROpConst(DReg, IRSub, BPReg, IRShort(((stackidx + 1)).toShort)))
            case (_, Some(s)) => List(
              RisROpConst(DReg, IRSub, BPReg, IRShort(((stackidx + s.toInt)).toShort)))
          }
        }
        case (_, i, _) if i >= 0 => List(
          RisROpConst(DReg, IRAdd, BPReg, IRShort((i + 2).toShort)))
        case (_, _, i) if i >= 0 => globals(i) match {
          case (s, _) => List(
            AisConst(IRLabel(s)),
            DisA)
        }
        case (_, _, _) => throw new Exception("var not found")
      }
      (moveFromD(d, intoD), usedTmp, 0)
    }
    case EDeref(EArrGet(e1, e2)) => {
      exprToIR(d, EAdd(e1, e2), usedRegs, usedTmp, globals, params, vars)

    }
    case EDeref(ERefer(e)) => {
      exprToIR(d, e, usedRegs, usedTmp, globals, params, vars)
    }
    case EDeref(_) => throw new Exception("no Deref")
    case EAdd(e1, e2) => {
      genOp(IRAdd, d, e1, e2, usedRegs, usedTmp, globals, params, vars)
    }
    case ESub(e1, e2) => {
      genOp(IRSub, d, e1, e2, usedRegs, usedTmp, globals, params, vars)
    }
    case EAnd(e1, e2) => {
      genOp(IRAnd, d, e1, e2, usedRegs, usedTmp, globals, params, vars)
    }
    case EOr(e1, e2) => {
      genOp(IROr, d, e1, e2, usedRegs, usedTmp, globals, params, vars)
    }
    case ERefer(e) => {
      val (e1eval, mt, mp) = exprToIR(DLoc, e, usedRegs, usedTmp, globals, params, vars)
      val intoD = e1eval ++ List(
        AisD,
        DisAref)
      (moveFromD(d, intoD), mt, mp)
    }
    case EArrGet(e1, e2) => {
      val (intoD, mt, mp) = genOp(IRAdd, DLoc, e1, e2, usedRegs, usedTmp, globals, params, vars)
      val nintoD = intoD ++ List(
        AisD,
        DisAref)
      (moveFromD(d, nintoD), mt, mp)

    }
    case ENeg(e) => {
      val (intoD, mt, mp) = exprToIR(DLoc, e, usedRegs, usedTmp, globals, params, vars)
      val nintoD = intoD ++ List(
        DisDOp(IRNeg))
      (moveFromD(d, nintoD), mt, mp)
    }
    case EMinus(e) => {
      val (intoD, mt, mp) = exprToIR(DLoc, e, usedRegs, usedTmp, globals, params, vars)
      val nintoD = intoD ++ List(
        DisDOp(IRMin))
      (moveFromD(d, nintoD), mt, mp)
    }
    case EPlus(e) => exprToIR(d, e, usedRegs, usedTmp, globals, params, vars)
    case EFunCall(name, args) => {
      val f = args.length
      val (argsInst, mt, mp) = (0 until f).map((i) =>
        exprToIR(SPLoc(i + 1), args(i), usedRegs, usedTmp, globals, params, vars)).foldLeft((List[EBIRStmt](), 0, 0)) {
        case ((i, t, p), (j, jt, jp)) => (i ++ j, t max jt, p max jp)
      }
      val saveInst = (0 until usedRegs).map((i) =>
        List(
          DisR(RReg(i)),
          RisD(RtReg),
          RisROpConst(DReg, IRSub, BPReg, IRShort(((i + stackSize(vars) + usedTmp + 1)).toShort)),
          RisD(Rt2Reg),
          DisR(RtReg),
          RefRisD(Rt2Reg))).foldLeft(List[EBIRStmt]()) {
        case (i, j) => i ++ j
      }
      val callInst = {
        val a = genLabel()
        List(
          AisConst(IRLabel(a)),
          DisA,
          RefRisD(SPReg),
          AisConst(IRLabel(name)),
          IRJMP,
          IRLabelStmt(a))
      }
      val restoreInst = (0 until usedRegs).map((i) =>
        List(
          RisROpConst(DReg, IRSub, BPReg, IRShort(((i + stackSize(vars) + usedTmp + 1)).toShort)),
          RisD(RReg(i)))).foldLeft(List[EBIRStmt]()) {
        case (i, j) => i ++ j
      }
      val intoD = (((argsInst ++ saveInst ++ callInst)) ++ restoreInst) :+ DisR(RtReg)
      (moveFromD(d, intoD), mt max usedRegs, mp max (args.length + 1))

    }
  }

  def genJump(a: String): List[EBIRStmt] = {
    List(
      AisConst(IRLabel(a)),
      IRJMP)
  }

  def bexprToIR(
    a1: String,
    jumpTrue: Boolean,
    e: EBCBExpr,
    globals: List[(String, Option[BigInt])],
    params: List[String],
    vars: List[(String, Option[BigInt])]): (List[EBIRStmt], Int, Int) = e match {
    case BNZero(e1) => {
      val (ep, mt, mp) = exprToIR(DLoc, e1, 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJNE else IRJEQ), mt, mp)
    }
    case BZero(e1) => {
      val (ep, mt, mp) = exprToIR(DLoc, e1, 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJEQ else IRJNE), mt, mp)
    }
    case BLT(e1, e2) => {
      val (ep, mt, mp) = exprToIR(DLoc, ESub(e1, e2), 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJLT else IRJGE), mt, mp)
    }
    case BGT(e1, e2) => {
      val (ep, mt, mp) = exprToIR(DLoc, ESub(e1, e2), 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJGT else IRJLE), mt, mp)
    }
    case BLE(e1, e2) => {
      val (ep, mt, mp) = exprToIR(DLoc, ESub(e1, e2), 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJLE else IRJGT), mt, mp)
    }
    case BGE(e1, e2) => {
      val (ep, mt, mp) = exprToIR(DLoc, ESub(e1, e2), 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJGE else IRJLT), mt, mp)
    }
    case BEQ(e1, e2) => {
      val (ep, mt, mp) = exprToIR(DLoc, ESub(e1, e2), 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJEQ else IRJNE), mt, mp)
    }
    case BNE(e1, e2) => {
      val (ep, mt, mp) = exprToIR(DLoc, ESub(e1, e2), 0, 0, globals, params, vars)
      (ep ++ List(
        AisConst(IRLabel(a1)),
        if (jumpTrue) IRJNE else IRJEQ), mt, mp)
    }
  }

  def stmtToIR(
    stmt: EBCStmt,
    globals: List[(String, Option[BigInt])],
    params: List[String],
    vars: List[(String, Option[BigInt])]): (List[EBIRStmt], Int, Int) = stmt match {
    case ExprStmt(e) => {
      exprToIR(DLoc, e, 0, 0, globals, params, vars)
    }
    case AssignStmt(EVarExp(v), e) => {
      val vi = vars indexWhere { case (x, y) => x == v }
      val pi = params indexWhere { case x => x == v }
      val gi = globals indexWhere { case (x, y) => x == v }
      val loc = (vi, pi, gi) match {
        case (i, _, _) if i >= 0 => {
          val stackidx = (vars take i).foldLeft(0) {
            case (i, (_, None)) => i + 1
            case (i, (_, Some(j))) => (i + j).toInt
          }
          vars(i) match {
            case (_, None) => {
              BPLoc(-(stackidx + 1))
            }
            case (_, Some(_)) => throw new Exception("cannot change array")
          }
        }
        case (_, i, _) if i >= 0 =>
          BPLoc(i + 2)
        case (_, _, i) if i >= 0 => {
          globals(i) match {
            case (s, None) => GlobalLoc(s)
            case (s, Some(_)) => throw new Exception("cannot change array")
          }
        }
        case (_, _, _) => throw new Exception("var not found")
      }
      exprToIR(loc, e, 0, 0, globals, params, vars)
    }
    case AssignStmt(EArrGet(e1, e2), e) => {
      val (d, nu, nt) = getFreshLoc(0, 0, stackSize(vars))
      val (pval, mt, mp) = exprToIR(d.d, EAdd(e1, e2), 0, 0, globals, params, vars)
      val (vval, mt2, mp2) = exprToIR(DLoc, e, nu, nt, globals, params, vars)
      val insts = d match {
        case Rtarg(RegLoc(i)) => pval ++ vval :+ RefRisD(RReg(i))
        case Mtarg(BPLoc(i)) => pval ++ vval ++ List(
          RisD(RtReg),
          DisR(BPReg),
          if (i > 0) AisDOpConst(IRAdd, IRShort(i.toShort))
          else AisDOpConst(IRSub, IRShort((-i).toShort)),
          DisAref,
          RisD(Rt2Reg),
          DisR(RtReg),
          RefRisD(Rt2Reg))
      }
      (insts, mt max mt2, mp max mp2)
    }
    case AssignStmt(ERefer(e1), e) => {
      val (d, nu, nt) = getFreshLoc(0, 0, stackSize(vars))
      val (pval, mt, mp) = exprToIR(d.d, e1, 0, 0, globals, params, vars)
      val (vval, mt2, mp2) = exprToIR(DLoc, e, nu, nt, globals, params, vars)
      val insts = d match {
        case Rtarg(RegLoc(i)) => pval ++ vval :+ RefRisD(RReg(i))
        case Mtarg(BPLoc(i)) => pval ++ vval ++ List(
          RisD(RtReg),
          DisR(BPReg),
          if (i > 0) AisDOpConst(IRAdd, IRShort(i.toShort))
          else AisDOpConst(IRSub, IRShort((-i).toShort)),
          DisAref,
          RisD(Rt2Reg),
          DisR(RtReg),
          RefRisD(Rt2Reg))
      }
      (insts, mt max mt2, mp max mp2)
    }
    case AssignStmt(_, _) => throw new Exception("Unintended assign")
    case IfStmt(cond, body, elseb) => {
      val a1 = genLabel()
      val (condval, mt, mp) = bexprToIR(a1, false, cond, globals, params, vars)
      val (trueinst, mt2, mp2) = stmtToIR(body, globals, params, vars)
      val (insts, mt4, mp4) = elseb match {
        case None => (condval ++ trueinst :+ IRLabelStmt(a1), mt max mt2, mp max mp2)
        case Some(ebody) => {
          val a3 = genLabel()
          val (elseinst, mt3, mp3) = stmtToIR(ebody, globals, params, vars)
          (condval ++ trueinst ++ genJump(a3) ++
            (IRLabelStmt(a1) +: elseinst) :+ IRLabelStmt(a3), mt max mt2 max mt3, mp max mp2 max mp3)
        }
      }
      (insts, mt4, mp4)
    }
    case ForStmt(init, cond, acc, body) => {
      val (initStmt, mt, mp) = init.foldLeft((List[EBIRStmt](), 0, 0)) {
        case ((i, ti, pi), j) => {
          val (ij, tj, pj) = stmtToIR(j, globals, params, vars)
          (i ++ ij, ti max tj, pi max pj)
        }
      }
      val (accStmt, mt2, mp2) = acc.foldLeft((List[EBIRStmt](), 0, 0)) {
        case ((i, ti, pi), j) => {
          val (ij, tj, pj) = stmtToIR(j, globals, params, vars)
          (i ++ ij, ti max tj, pi max pj)
        }
      }
      val (bodyStmt, mt3, mp3) = stmtToIR(body, globals, params, vars)
      val a1 = genLabel()
      val a2 = genLabel()
      val (condStmt, mt4, mp4) = cond match {
        case Some(i) => bexprToIR(a2, false, i, globals, params, vars)
        case None => (List(), 0, 0)
      }
      (initStmt ++ (IRLabelStmt(a1) +: (condStmt ++ bodyStmt ++ accStmt ++ genJump(a1))) :+ IRLabelStmt(a2),
        mt max mt2 max mt3 max mt4,
        mp max mp2 max mp3 max mp4)
    }
    case WhileStmt(cond, body) => {
      val a1 = genLabel()
      val a2 = genLabel()
      val (condStmt, mt, mp) = bexprToIR(a2, false, cond, globals, params, vars)
      val (bodyStmt, mt2, mp2) = stmtToIR(body, globals, params, vars)
      (IRLabelStmt(a1) +: (condStmt ++ bodyStmt ++ genJump(a1)) :+ IRLabelStmt(a2), mt max mt2, mp max mp2)
    }
    case ReturnStmt(e) => {
      val (evalue, mt, mp) = exprToIR(DLoc, e, 0, 0, globals, params, vars)
      (evalue ++
        List(
          RisD(RtReg),
          RisROpConst(DReg, IRAdd, BPReg, IRShort(1.toShort)),
          AisD,
          DisAref,
          RisD(Rt2Reg),
          RisROpConst(DReg, IRAdd, BPReg, IRShort(1.toShort)),
          RisD(SPReg),
          DisR(BPReg),
          AisD,
          DisAref,
          RisD(BPReg),
          DisR(Rt2Reg),
          AisD,
          IRJMP), mt, mp) /*
      Rt <- BP + 1
      SP <- &BP + 1
      BP <- BP*/
    }
    case BlockStmt(l) => l.foldLeft((List[EBIRStmt](), 0, 0)) {
      case ((i, ti, pi), j) => {
        val (ij, tj, pj) = stmtToIR(j, globals, params, vars)
        (i ++ ij, ti max tj, pi max pj)
      }
    }
  }

  def prolog(p: String, k: Int) = List(
    IRLabelStmt(p),
    RisROpConst(RtReg, IRSub, SPReg, IRShort((1).toShort)),
    DisR(BPReg),
    RefRisD(RtReg),
    RisROpConst(BPReg, IRSub, SPReg, IRShort((1).toShort)),
    RisROpConst(SPReg, IRSub, SPReg, IRShort(((k + 2)).toShort)) /*
    SP - 1 <- BP
    BP <- SP - 1
    SP <- SP - 1 - k
    */ )
  def epilog = List(
    RisD(RtReg),
    RisROpConst(DReg, IRAdd, BPReg, IRShort(1.toShort)),
    AisD,
    DisAref,
    RisD(Rt2Reg),
    RisROpConst(DReg, IRAdd, BPReg, IRShort(1.toShort)),
    RisD(SPReg),
    DisR(BPReg),
    AisD,
    DisAref,
    RisD(BPReg),
    DisR(Rt2Reg),
    AisD,
    IRJMP)

  def fundefToIR(pdef: Map[String, (Option[BigInt], EBCType)], fdef: Map[String, EBCFun]): Map[String, EBIRFun] = {
    val plist = pdef.toList.map { case (x, (y, z)) => (x, y) }
    fdef.map {
      case (k, EBCFun(params, vars, _, body)) => {
        val pm = params.map { case (x, y) => x }
        val vm = vars.map { case (x, y, z) => (x, y) }
        val fbody: List[EBIRStmt] = List()
        val (irlist, mt, mp) = body.foldLeft((fbody, 0, 0)) {
          case ((irlist, mt, mp), i) => {
            val (irstmt, mt2, mp2) = stmtToIR(i, plist, pm, vm)
            (irlist ++ irstmt, mt max mt2, mp max mp2)
          }
        }
        (k -> EBIRFun(prolog(k, mt + mp + stackSize(vm)) ++ irlist ++ epilog))
      }
    }
  }

  def progToIR(eprog: EBCProg): EBIRProg = eprog match {
    case EBCProg(predef, fundef) => {
      val preIR = predefToIR(predef)
      val funIR = fundefToIR(predef.map { case (k, (i, j, _)) => (k -> (i, j)) }, fundef)
      EBIRProg(preIR, funIR)
    }
  }

  def apply(eprog: EBCProg): EBAssembly = progToIR(eprog).toAsm

}
case object EBCCompilerIntegration {

  def toVector(f: Short): Vector[Boolean] = {
    (0 until 16).map((i) => if ((f & (1 << i)) == 0) false else true).toVector
  }

  def apply(f: String) = {
    EBCParserIntegration(Source.fromFile(f).mkString).right.map {
      case i => {
        val nb = EBCCompiler(i).bootStrap
        println(nb)
        val (rom, ram) = nb.toBin
        rom.map(toVector(_)).reduce(_ ++ _) ++ ram.map(toVector(_)).reduce(_ ++ _)
      }
    }
  }
}

sealed trait ExprLoc
case object DLoc extends ExprLoc
case class RegLoc(i: Int) extends ExprLoc

sealed trait MemoryLoc extends ExprLoc
case class SPLoc(i: Int) extends MemoryLoc
case class BPLoc(i: Int) extends MemoryLoc
case class GlobalLoc(s: String) extends MemoryLoc

sealed trait TargetLoc {
  val d: ExprLoc
}
case class Rtarg(i: RegLoc) extends TargetLoc {
  val d = i
}
case class Mtarg(i: BPLoc) extends TargetLoc {
  val d = i
}

case class ExprChunk(l: ExprLoc, c: List[EBAInst])
