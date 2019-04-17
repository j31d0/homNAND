package HomNAND

import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait EBCToken

case class IDENTIFIER(str: String) extends EBCToken
case class LITERAL(int: BigInt) extends EBCToken
case class STRLITERAL(str: String) extends EBCToken
case object LPAREN extends EBCToken
case object RPAREN extends EBCToken
case object LCURL extends EBCToken
case object RCURL extends EBCToken
case object LSQUARE extends EBCToken
case object RSQUARE extends EBCToken
case object COMMA extends EBCToken
case object SEMICOLON extends EBCToken
case object ASTERIC extends EBCToken
case object AND extends EBCToken
case object OR extends EBCToken
case object PLUS extends EBCToken
case object MINUS extends EBCToken
case object NOT extends EBCToken
case object NEG extends EBCToken
case object ASSIGN extends EBCToken
case object EQ extends EBCToken
case object LT extends EBCToken
case object GT extends EBCToken
case object LE extends EBCToken
case object GE extends EBCToken
case object NE extends EBCToken

case object TYPESHORT extends EBCToken
case object IF extends EBCToken
case object ELSE extends EBCToken
case object FOR extends EBCToken
case object WHILE extends EBCToken
case object RETURN extends EBCToken

trait EBCCompilationError
case class EBCLexerError(msg: String) extends EBCCompilationError {
  override def toString = s"[Lexer Error] : $msg"
}
case class EBCParserError(msg: String) extends EBCCompilationError {
  override def toString = s"[Parser Error] : $msg"
}

object EBCLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f\n]+".r

  def strliteral: Parser[STRLITERAL] = {
    "\"(?:[^\"\\\\]|\\\\.)*\"".r ^^ { str =>
      {
        val ns = str.replace("\\\"", "\"")
        STRLITERAL(ns.substring(1, ns.length - 1))
      }
    }
  }

  def charliteral: Parser[LITERAL] = {
    "'([^'\\\\\\n]|\\\\[\\\\'])'".r ^^ { str =>
      {
        val ns = str.substring(1, str.length - 1).replace("\\'", "'").replace("\\\\", "\\")
        LITERAL(BigInt(ns.charAt(0)))
      }
    }
  }

  def varidentifier: Parser[IDENTIFIER] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def literal: Parser[LITERAL] = {
    "[0-9]+".r ^^ { str => LITERAL(BigInt(str)) }
  }

  def lparen = "(" ^^ { _ => LPAREN }
  def rparen = ")" ^^ { _ => RPAREN }
  def lcurl = "{" ^^ { _ => LCURL }
  def rcurl = "}" ^^ { _ => RCURL }
  def lsquare = "[" ^^ { _ => LSQUARE }
  def rsquare = "]" ^^ { _ => RSQUARE }
  def comma = "," ^^ { _ => COMMA }
  def semicolon = ";" ^^ { _ => SEMICOLON }
  def asteric = "*" ^^ { _ => ASTERIC }
  def and = "&" ^^ { _ => AND }
  def or = "|" ^^ { _ => OR }
  def plus = "+" ^^ { _ => PLUS }
  def minus = "-" ^^ { _ => MINUS }
  def not = "!" ^^ { _ => NOT }
  def neg = "~" ^^ { _ => NEG }
  def assign = "=" ^^ { _ => ASSIGN }
  def eq = "==" ^^ { _ => EQ }
  def ne = "!=" ^^ { _ => NE }
  def lt = "<" ^^ { _ => LT }
  def gt = ">" ^^ { _ => GT }
  def le = "<=" ^^ { _ => LE }
  def ge = ">=" ^^ { _ => GE }
  def tshort = "short" ^^ { _ => TYPESHORT }
  def sif = "if" ^^ { _ => IF }
  def selse = "else" ^^ { _ => ELSE }
  def sfor = "for" ^^ { _ => FOR }
  def swhile = "while" ^^ { _ => WHILE }
  def sreturn = "return" ^^ { _ => RETURN }

  def tokens: Parser[List[EBCToken]] = {
    phrase(rep1(lparen | rparen | lcurl | rcurl | lsquare | rsquare | comma |
      semicolon | asteric | and | or | plus | minus | neg |
      ne | eq | assign | le | ge | lt | gt | not | tshort | sif | selse | sfor | swhile | sreturn |
      literal | charliteral | strliteral | varidentifier))
  }

  def apply(code: String): Either[EBCLexerError, List[EBCToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(EBCLexerError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

sealed trait Op1
case object SPlus extends Op1
case object SMinus extends Op1
case object SNeg extends Op1
case object SAsteric extends Op1
case object SAnd extends Op1

sealed trait Op2
case object MPlus extends Op2
case object MMinus extends Op2

object EBCParser extends PackratParsers {
  override type Elem = EBCToken

  class EBCTokenReader(tokens: Seq[EBCToken]) extends Reader[EBCToken] {
    override def first: EBCToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[EBCToken] = new EBCTokenReader(tokens.tail)
  }

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifer", { case id @ IDENTIFIER(name) => id })
  }

  private def literal: Parser[LITERAL] = {
    accept("Int literal", { case lit @ LITERAL(value) => lit })
  }

  private def strliteral: Parser[STRLITERAL] = {
    accept("Str literal", { case lit @ STRLITERAL(str) => lit })
  }

  def deftype: Parser[EBCType] = {
    val tshort = TYPESHORT ^^ { _ => TShort() }
    tshort
  }

  def defvar: Parser[(String, EBCType)] = {
    deftype ~ identifier ^^ {
      case ty ~ IDENTIFIER(name) => (name, ty)
    }
  }

  def arrindex: Parser[BigInt] = {
    LSQUARE ~ literal ~ RSQUARE ^^ { case _ ~ LITERAL(value) ~ _ => value }
  }

  def sign: Parser[Boolean] = {
    (PLUS ^^ { _ => true }) | (MINUS ^^ { _ => false })
  }

  def initValue: Parser[EBCInitValue] = {
    val strconst = strliteral ^^ { case STRLITERAL(s) => ConstStr(s) }
    val intvalue = opt(sign) ~ literal ^^ {
      case None ~ LITERAL(s) => s
      case Some(true) ~ LITERAL(s) => s
      case Some(false) ~ LITERAL(s) => -s
    }
    val intconst = intvalue ^^ { case s => ConstInt(s) }
    val intlistval = rep(intvalue ~ COMMA ^^ { case f ~ _ => f }) ~ intvalue ^^ {
      case l1 ~ o => l1 :+ o
    }
    val arrayconst = LCURL ~ opt(intlistval) ~ RCURL ^^ {
      case _ ~ lopt ~ _ => ConstArr(lopt.getOrElse(List()))
    }
    strconst | constValue | arrayconst
  }

  def constValue: Parser[EBCConstValue] = {
    val intvalue = opt(sign) ~ literal ^^ {
      case None ~ LITERAL(s) => s
      case Some(true) ~ LITERAL(s) => s
      case Some(false) ~ LITERAL(s) => -s
    }
    val intconst = intvalue ^^ { case s => ConstInt(s) }
    intconst
  }

  def constAssign: Parser[EBCConstValue] = {
    ASSIGN ~ constValue ^^ { case _ ~ cv => cv }
  }

  def initAssign: Parser[EBCInitValue] = {
    ASSIGN ~ initValue ^^ { case _ ~ cv => cv }
  }

  def predef: Parser[(String, (Option[BigInt], EBCType, Option[EBCInitValue]))] = {
    defvar ~ opt(arrindex) ~ opt(initAssign) ~ SEMICOLON ^^ {
      case (name, ty) ~ capopt ~ valueopt ~ _ => (name, (capopt, ty, valueopt))
    }
  }

  def paramList: Parser[List[(String, EBCType)]] = {
    rep(defvar ~ COMMA ^^ { case f ~ _ => f }) ~ defvar ^^ {
      case l1 ~ o => l1 :+ o
    }
  }

  def exprAssign: Parser[EBCExpr] = {
    ASSIGN ~ expr ^^ { case _ ~ ex => ex }
  }

  def vardef: Parser[(String, (Option[BigInt], EBCType, Option[EBCExpr]))] = {
    defvar ~ opt(arrindex) ~ opt(exprAssign) ~ SEMICOLON ^^ {
      case (name, ty) ~ capopt ~ valueopt ~ _ => (name, (capopt, ty, valueopt))
    }
  }
  def argList: Parser[List[EBCExpr]] = {
    rep(expr ~ COMMA ^^ { case f ~ _ => f }) ~ expr ^^ {
      case l1 ~ o => l1 :+ o
    }
  }

  def parenexp: Parser[EBCExpr] = {
    LPAREN ~ expr ~ RPAREN ^^ { case _ ~ e ~ _ => e }
  }
  def varexp: Parser[EVarExp] = {
    identifier ^^ { case IDENTIFIER(s) => EVarExp(s) }
  }
  def constexp: Parser[EConstExp] = {
    constValue ^^ { case v => EConstExp(v) }
  }

  def expr0: Parser[EBCExpr] = {
    varexp | constexp | parenexp
  }
  def funcall: Parser[EFunCall] = {
    identifier ~ LPAREN ~ opt(argList) ~ RPAREN ^^ {
      case IDENTIFIER(s) ~ _ ~ argop ~ _ => EFunCall(s, argop.getOrElse(List()))
    }
  }
  def arrget: Parser[EArrGet] = {
    (funcall | expr0) ~ LSQUARE ~ expr ~ RSQUARE ~ rep(LSQUARE ~ expr ~ RSQUARE ^^ { case _ ~ e ~ _ => e }) ^^ {
      case e1 ~ _ ~ e2 ~ _ ~ el => el.foldLeft(EArrGet(e1, e2)) {
        case (eg, e) => EArrGet(eg, e)
      }
    }
  }

  def expr1: Parser[EBCExpr] = funcall | arrget | expr0

  def op1par: Parser[Op1] = {
    PLUS ^^ { _ => SPlus } |
      MINUS ^^ { _ => SMinus } |
      NEG ^^ { _ => SNeg } |
      ASTERIC ^^ { _ => SAsteric } |
      AND ^^ { _ => SAnd }
  }

  def expr2: Parser[EBCExpr] = {
    rep(op1par) ~ expr1 ^^ {
      case l ~ ei => l.foldRight(ei) {
        case (SPlus, e) => EPlus(e)
        case (SMinus, e) => EMinus(e)
        case (SNeg, e) => ENeg(e)
        case (SAsteric, e) => ERefer(e)
        case (SAnd, e) => EDeref(e)
      }
    }
  }

  def refexp: Parser[ERefer] = {
    ASTERIC ~ expr2 ^^ { case _ ~ e => ERefer(e) }
  }

  def op2par: Parser[Op2] = {
    PLUS ^^ { _ => MPlus } |
      MINUS ^^ { _ => MMinus }
  }

  def expr3: Parser[EBCExpr] = {
    expr2 ~ rep(op2par ~ expr2 ^^ { case op ~ e => (op, e) }) ^^ {
      case ei ~ l => l.foldLeft(ei) {
        case (e, (MPlus, e2)) => EAdd(e, e2)
        case (e, (MMinus, e2)) => ESub(e, e2)
      }
    }
  }

  def expr4: Parser[EBCExpr] = {
    expr3 ~ rep(AND ~ expr3 ^^ { case _ ~ e => e }) ^^ {
      case ei ~ l => l.foldLeft(ei) {
        case (e, e2) => EAnd(e, e2)
      }
    }
  }

  def expr5: Parser[EBCExpr] = {
    expr4 ~ rep(OR ~ expr4 ^^ { case _ ~ e => e }) ^^ {
      case ei ~ l => l.foldLeft(ei) {
        case (e, e2) => EOr(e, e2)
      }
    }
  }

  def assignable: Parser[EBCAssign] = varexp | arrget | refexp

  def expr: Parser[EBCExpr] = {
    expr5
  }

  def bnzero: Parser[BNZero] = expr ^^ { case e => BNZero(e) }
  def bzero: Parser[BZero] = NOT ~ expr ^^ { case _ ~ e => BZero(e) }
  def blt: Parser[BLT] = expr ~ LT ~ expr ^^ { case e1 ~ _ ~ e2 => BLT(e1, e2) }
  def bgt: Parser[BGT] = expr ~ GT ~ expr ^^ { case e1 ~ _ ~ e2 => BGT(e1, e2) }
  def ble: Parser[BLE] = expr ~ LE ~ expr ^^ { case e1 ~ _ ~ e2 => BLE(e1, e2) }
  def bge: Parser[BGE] = expr ~ GE ~ expr ^^ { case e1 ~ _ ~ e2 => BGE(e1, e2) }
  def beq: Parser[BEQ] = expr ~ EQ ~ expr ^^ { case e1 ~ _ ~ e2 => BEQ(e1, e2) }
  def bne: Parser[BNE] = expr ~ NE ~ expr ^^ { case e1 ~ _ ~ e2 => BNE(e1, e2) }
  def bexpr: Parser[EBCBExpr] = blt | bgt | ble | bge | beq | bne | bnzero | bzero

  def ifstmt: Parser[IfStmt] = {
    IF ~ LPAREN ~ bexpr ~ RPAREN ~ stmt ~ opt(ELSE ~ stmt ^^ { case _ ~ s => s }) ^^ {
      case _ ~ _ ~ be ~ _ ~ s ~ os => IfStmt(be, s, os)
    }
  }

  def assignstmt_list: Parser[List[AssignStmt]] = {
    rep(assignable ~ ASSIGN ~ expr ~ COMMA ^^ {
      case e1 ~ _ ~ e2 ~ _ => AssignStmt(e1, e2)
    }) ~ assignable ~ ASSIGN ~ expr ^^ {
      case l1 ~ e1 ~ _ ~ e2 => l1 :+ AssignStmt(e1, e2)
    }
  }

  def assignstmt: Parser[AssignStmt] = {
    assignable ~ ASSIGN ~ expr ~ SEMICOLON ^^ {
      case e1 ~ _ ~ e2 ~ _ => AssignStmt(e1, e2)
    }
  }

  def forstmt: Parser[ForStmt] = {
    FOR ~ LPAREN ~ opt(assignstmt_list) ~ SEMICOLON ~ opt(bexpr) ~ SEMICOLON ~
      opt(assignstmt_list) ~ RPAREN ~ stmt ^^ {
        case _ ~ _ ~ aop ~ _ ~ bop ~ _ ~ uop ~ _ ~ body => ForStmt(
          aop.getOrElse(List()),
          bop,
          uop.getOrElse(List()),
          body)
      }
  }

  def whilestmt: Parser[WhileStmt] = {
    WHILE ~ LPAREN ~ bexpr ~ RPAREN ~ stmt ^^ {
      case _ ~ _ ~ be ~ _ ~ s => WhileStmt(be, s)
    }
  }

  def exprstmt: Parser[ExprStmt] = {
    expr ~ SEMICOLON ^^ { case e ~ _ => ExprStmt(e) }
  }

  def returnstmt: Parser[ReturnStmt] = {
    RETURN ~ expr ~ SEMICOLON ^^ { case _ ~ e ~ _ => ReturnStmt(e) }
  }

  def blockstmt: Parser[BlockStmt] = {
    LCURL ~ rep(stmt) ~ RCURL ^^ { case _ ~ l ~ _ => BlockStmt(l) }
  }

  def stmt: Parser[EBCStmt] = {
    ifstmt | assignstmt | forstmt | whilestmt | exprstmt | returnstmt | blockstmt
  }

  def fundef: Parser[(String, EBCFun)] = {
    defvar ~ LPAREN ~ opt(paramList) ~ RPAREN ~ LCURL ~ rep(vardef) ~ rep(stmt) ~ RCURL ^^ {
      case (fname, ty) ~ _ ~ popt ~ _ ~ _ ~ defs ~ stmts ~ _ => {
        val (dec, assg) = defs.foldLeft(
          (List[(String, Option[BigInt], EBCType)](), List[EBCStmt]())) {
            case ((d, a), (name, (aop, ty, exop))) => (d :+ (name, aop, ty), exop match {
              case None => a
              case Some(ex) => a :+ AssignStmt(EVarExp(name), ex)
            })
          }
        popt match {
          case None => (fname, EBCFun(List(), dec, ty, assg ++ stmts))
          case Some(l) => (fname, EBCFun(l, dec, ty, assg ++ stmts))
        }
      }
    }
  }

  def program: Parser[EBCProg] = phrase(rep(predef) ~ rep(fundef)) ^^ {
    case pd ~ fd => EBCProg(pd.toMap, fd.toMap)
  }

  def apply(tokens: Seq[EBCToken]): Either[EBCParserError, EBCProg] = {
    val reader = new EBCTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(EBCParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

object EBCParserIntegration {
  def apply(code: String): Either[EBCCompilationError, EBCProg] = {
    for {
      tokens <- EBCLexer(code).right
      cmd <- EBCParser(tokens).right
    } yield cmd
  }
}
