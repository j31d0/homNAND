package HomNAND

case class EBCProg(predef: Map[String, (Option[BigInt], EBCType, Option[EBCInitValue])], fundef: Map[String, EBCFun])

sealed trait EBCType
case class TShort() extends EBCType
case class TPointer(v: EBCType) extends EBCType

sealed trait EBCInitValue
case class ConstStr(value: String) extends EBCInitValue
case class ConstArr(value: List[BigInt]) extends EBCInitValue

sealed trait EBCConstValue extends EBCInitValue
case class ConstInt(value: BigInt) extends EBCConstValue

case class EBCFun(params: List[(String, EBCType)], vars: List[(String, Option[BigInt], EBCType)], rett: EBCType, body: List[EBCStmt])

sealed trait EBCStmt
case class IfStmt(cond: EBCBExpr, body: EBCStmt, elseb: Option[EBCStmt]) extends EBCStmt
case class ForStmt(init: List[AssignStmt], cond: Option[EBCBExpr], acc: List[AssignStmt], body: EBCStmt ) extends EBCStmt
case class WhileStmt(cond: EBCBExpr, body: EBCStmt ) extends EBCStmt
case class AssignStmt(left: EBCAssign, right: EBCExpr) extends EBCStmt
case class ExprStmt(e: EBCExpr) extends EBCStmt
case class ReturnStmt(e: EBCExpr) extends EBCStmt
case class BlockStmt(st: List[EBCStmt]) extends EBCStmt

sealed trait EBCExpr
case class EConstExp(v: EBCConstValue) extends EBCExpr
case class EAdd(e1: EBCExpr, e2: EBCExpr) extends EBCExpr
case class ESub(e1: EBCExpr, e2: EBCExpr) extends EBCExpr
case class EAnd(e1: EBCExpr, e2: EBCExpr) extends EBCExpr
case class EOr(e1: EBCExpr, e2: EBCExpr) extends EBCExpr
case class ENeg(e: EBCExpr) extends EBCExpr
case class EMinus(e: EBCExpr) extends EBCExpr
case class EPlus(e: EBCExpr) extends EBCExpr
case class EDeref(e: EBCExpr) extends EBCExpr
case class EFunCall(name: String, args: List[EBCExpr]) extends EBCExpr

sealed trait EBCBExpr
case class BNZero(e: EBCExpr) extends EBCBExpr
case class BZero(e: EBCExpr) extends EBCBExpr
case class BLT(e1: EBCExpr, e2: EBCExpr) extends EBCBExpr
case class BGT(e1: EBCExpr, e2: EBCExpr) extends EBCBExpr
case class BLE(e1: EBCExpr, e2: EBCExpr) extends EBCBExpr
case class BGE(e1: EBCExpr, e2: EBCExpr) extends EBCBExpr
case class BEQ(e1: EBCExpr, e2: EBCExpr) extends EBCBExpr
case class BNE(e1: EBCExpr, e2: EBCExpr) extends EBCBExpr

sealed trait EBCAssign extends EBCExpr
case class EVarExp(name: String) extends EBCAssign
case class EArrGet(e1: EBCExpr, e2: EBCExpr) extends EBCAssign
case class ERefer(e: EBCExpr) extends EBCAssign
