package HomNAND
import scala.util.Random
import java.io._
import scala.io.Source
import sys.process._

object EncFactory {

  def genConst(f: Byte): Vector[EBitSym] = {
    (0 until 4).map((i) => if ((f & (1 << i)) == 0) SymConstBit(false) else SymConstBit(true)).toVector
  }
  def genShort(f: Short): Vector[EBitSym] = {
    (0 until 16).map((i) => if ((f & (1 << i)) == 0) SymConstBit(false) else SymConstBit(true)).toVector
  }
  def substitution(k: Vector[EBitSym]): Vector[EBitSym] = {
    val p = List(3, 5, 15, 10, 0, 1, 7, 12, 11, 8, 6, 2, 13, 14, 9, 4)
    val sp = p.map((i) => genConst(i.toByte))
    (k grouped 4).toVector.map((i) => mux16way16(
      sp(0), sp(1), sp(2), sp(3),
      sp(4), sp(5), sp(6), sp(7),
      sp(8), sp(9), sp(10), sp(11),
      sp(12), sp(13), sp(14), sp(15),
      i)).reduce(_ ++ _)
  }

  def perm(k: Vector[EBitSym]): Vector[EBitSym] = {
    val p = List(0, 5, 15, 10, 3, 1, 7, 12, 11, 8, 6, 2, 13, 14, 9, 4)
    Vector.tabulate(16)((i) => k(p(i)))
  }

  def isubstitution(k: Vector[EBitSym]): Vector[EBitSym] = {
    val p = List(4, 5, 11, 0, 15, 1, 10, 6, 9, 14, 3, 8, 7, 12, 13, 2)
    val sp = p.map((i) => genConst(i.toByte))
    (k grouped 4).toVector.map((i) => mux16way16(
      sp(0), sp(1), sp(2), sp(3),
      sp(4), sp(5), sp(6), sp(7),
      sp(8), sp(9), sp(10), sp(11),
      sp(12), sp(13), sp(14), sp(15),
      i)).reduce(_ ++ _)
  }

  def mux(a: EBitSym, b: EBitSym, s: EBitSym): EBitSym = {
    SymOrBit(SymAndBit(a, SymNotBit(s).reduce).reduce, SymAndBit(b, s).reduce).reduce
  }

  def mux16(a: Vector[EBitSym], b: Vector[EBitSym], s: EBitSym): Vector[EBitSym] = {
    (a zip b).map { case (b1, b2) => mux(b1, b2, s) }
  }
  def mux4way16(
    a: Vector[EBitSym], b: Vector[EBitSym],
    c: Vector[EBitSym], d: Vector[EBitSym],
    s: Vector[EBitSym]): Vector[EBitSym] = {
    val w0 = mux16(a, b, s(0))
    val w1 = mux16(c, d, s(0))
    mux16(w0, w1, s(1))
  }

  def mux8way16(
    a: Vector[EBitSym], b: Vector[EBitSym],
    c: Vector[EBitSym], d: Vector[EBitSym],
    e: Vector[EBitSym], f: Vector[EBitSym],
    g: Vector[EBitSym], h: Vector[EBitSym],
    s: Vector[EBitSym]): Vector[EBitSym] = {
    val w0 = mux4way16(a, b, c, d, s take 2)
    val w1 = mux4way16(e, f, g, h, s take 2)
    mux16(w0, w1, s(2))
  }

  def mux16way16(
    a: Vector[EBitSym], b: Vector[EBitSym],
    c: Vector[EBitSym], d: Vector[EBitSym],
    e: Vector[EBitSym], f: Vector[EBitSym],
    g: Vector[EBitSym], h: Vector[EBitSym],
    a1: Vector[EBitSym], b1: Vector[EBitSym],
    c1: Vector[EBitSym], d1: Vector[EBitSym],
    e1: Vector[EBitSym], f1: Vector[EBitSym],
    g1: Vector[EBitSym], h1: Vector[EBitSym],
    s: Vector[EBitSym]): Vector[EBitSym] = {
    val w0 = mux8way16(a, b, c, d, e, f, g, h, s take 3)
    val w1 = mux8way16(a1, b1, c1, d1, e1, f1, g1, h1, s take 3)
    mux16(w0, w1, s(3))
  }

  def dec(a: Vector[EBitSym], key: Vector[EBitSym]) = {
    //val k = isubstitution(a)
    val kb = (a zip key).map{ case (i, j) => i xor j}
    kb
    //k
  }
  def enc(da: Vector[EBitSym], key: Vector[EBitSym]) = {
    val db = (da zip key).map{ case (i, j) => i xor j}
    db
    //val dk = substitution(da)
    //dk
  }

  def genHNand(key: Vector[EBitSym]): Vector[EBitSym] = {
    val a = Vector.tabulate(16)((n) => SymLocBit(n))
    val b = Vector.tabulate(16)((n) => SymLocBit(n + 16))
    val deca = dec(a, key)
    val decb = dec(b, key)
    val (ap, al) = (deca.head, deca.tail)
    val (bp, bl) = (decb.head, decb.tail)
    val decanb = perm((ap nand bp) +: (al zip bl).map { case (i, j) => (i xor j) })
    enc(decanb, key)
  }

  def prepare(ex: Vector[EBitSym]): EFastCircuit = {
    val k = ex.map(_.toExpr)
    val tmpString = Source.fromFile("./libcircuit/HomNAND_EFastCircuit_template.c").mkString
    val kl = k.length
    val circuit = (0 until k.length).map((i) => s"out[$i] = ${k(i)};\n").reduce(_ + _)
    val newString = tmpString.replace("$N", kl.toString).replace("$Circuit", circuit)
    val writer = new PrintWriter(new File("./libcircuit/HomNAND_EFastCircuit.c"))
    writer.print(newString)
    writer.close()
    "make -C libcircuit".!
    System.loadLibrary("EFastCircuit")
    new EFastCircuit
  }
}

case class HBit(v: Vector[Boolean])

object HBitNand extends EBitNand {
  type T = HBit
  val bsize = 16
  val key: Short = 0xfe92.toShort
  val rseed = Random
  lazy val homNand = EncFactory.prepare(EncFactory.genHNand(EncFactory.genShort(key)))
  val efalse: HBit = HBit(EncFactory.enc(EncFactory.genShort(31322.toShort), EncFactory.genShort(key)).map((f) => f.eval(Vector())))
  val etrue: HBit = HBit(EncFactory.enc(EncFactory.genShort(26585.toShort), EncFactory.genShort(key)).map((f) => f.eval(Vector())))
  def apply(a: Boolean): T = HBit(EncFactory.enc(Vector.tabulate(bsize)((n) => SymLocBit(n)), EncFactory.genShort(key)).map((f) => f.eval(a +: Vector.tabulate(bsize - 1)((n) => rseed.nextBoolean))))
  def decrypt(t: HBit): Boolean = EncFactory.dec(Vector.tabulate(bsize)((n) => SymLocBit(n)), EncFactory.genShort(key)).map((f) => f.eval(t.v)) apply 0
  def nand(a: HBit, b: HBit): HBit = (a, b) match {
    case (HBit(i), HBit(j)) => HBit(homNand.fastnand((i ++ j).toArray).toVector)
  }
  def toByte(l: Vector[Boolean]): Byte = {
    (0 until 8).foldLeft(0) {
      case (i, j) => if (l(j)) i | (1 << j) else i
    }.toByte
  }
  def toBytes(fvec: T): List[Byte] = {
    (fvec.v grouped 8).map(toByte).toList
  }

  def fromByte(f: Byte): Vector[Boolean] = {
    Vector.tabulate(8)((n) => (f & (1 << n)) != 0)
  }

  def fromBytes(f: List[Byte]): T = {
    HBit(fromByte(f(0)) ++ fromByte(f(1)))
  }

  def exportP(fvec: Vector[T]): Array[Byte] = {
    fvec.foldLeft(Vector[Byte]()) {
      case (i, j) => i ++ toBytes(j)
    }.toArray
  }

  def importP(farr: Array[Byte]): Vector[T] = {
    (farr.toList grouped 2).map((i) => fromBytes(i)).foldLeft(Vector[T]()) {
      case (v1, v2) => v1 :+ v2
    }
  }
}

object HBitLogic extends EBitLogic(HBitNand) {
  override val en = HBitNand

  override def and(a: HBit, b: HBit): HBit = (a, b) match {
    case (HBit(i), HBit(j)) => HBit(en.homNand.fastand((i ++ j).toArray).toVector)
  }
  override def or(a: HBit, b: HBit): HBit = (a, b) match {
    case (HBit(i), HBit(j)) => HBit(en.homNand.fastor((i ++ j).toArray).toVector)
  }
  override def xor(a: HBit, b: HBit): HBit = (a, b) match {
    case (HBit(i), HBit(j)) => HBit(en.homNand.fastxor((i ++ j).toArray).toVector)
  }
  override def not(a: HBit): HBit = a match {
    case HBit(i) => HBit(en.homNand.fastnot(i.toArray).toVector)
  }
  override def mux(a: HBit, b: HBit, s: HBit): HBit = (a, b, s) match {
    case (HBit(i), HBit(j), HBit(k)) => HBit(en.homNand.fastmux((i ++ j ++ k).toArray).toVector)
  }

  override def mux16(a: Vector[HBit], b: Vector[HBit], s: HBit): Vector[HBit] = {
    val ao = a.map { case HBit(i) => i }.reduce(_ ++ _)
    val bo = b.map { case HBit(i) => i }.reduce(_ ++ _)
    val f = (en.homNand.fastmux16((ao ++ bo ++ s.v).toArray).toVector grouped en.bsize).map(HBit(_)).toVector
    f
  }

  override def mux4way16(
    a: Vector[en.T], b: Vector[en.T],
    c: Vector[en.T], d: Vector[en.T],
    s: Vector[en.T]): Vector[en.T] = {

    val av = a.map { case HBit(i) => i }.reduce(_ ++ _)
    val bv = b.map { case HBit(i) => i }.reduce(_ ++ _)
    val cv = c.map { case HBit(i) => i }.reduce(_ ++ _)
    val dv = d.map { case HBit(i) => i }.reduce(_ ++ _)
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)

    val f = (en.homNand.fastmux4way16((av ++ bv ++ cv ++ dv ++ sv).toArray).toVector grouped en.bsize).map(HBit(_)).toVector
    f
  }

  override def mux8way16(
    a: Vector[en.T], b: Vector[en.T],
    c: Vector[en.T], d: Vector[en.T],
    e: Vector[en.T], f: Vector[en.T],
    g: Vector[en.T], h: Vector[en.T],
    s: Vector[en.T]): Vector[en.T] = {

    val av = a.map { case HBit(i) => i }.reduce(_ ++ _)
    val bv = b.map { case HBit(i) => i }.reduce(_ ++ _)
    val cv = c.map { case HBit(i) => i }.reduce(_ ++ _)
    val dv = d.map { case HBit(i) => i }.reduce(_ ++ _)

    val ev = e.map { case HBit(i) => i }.reduce(_ ++ _)
    val fv = f.map { case HBit(i) => i }.reduce(_ ++ _)
    val gv = g.map { case HBit(i) => i }.reduce(_ ++ _)
    val hv = h.map { case HBit(i) => i }.reduce(_ ++ _)
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)

    val res = (en.homNand.fastmux8way16((av ++ bv ++ cv ++ dv ++ ev ++ fv ++ gv ++ hv ++ sv).toArray).toVector grouped en.bsize).map(HBit(_)).toVector
    res
  }

  override def dmux(i: HBit, s: HBit): Vector[HBit] = {
    (en.homNand.fastdmux((i.v ++ s.v).toArray).toVector grouped en.bsize).map(HBit(_)).toVector
  }

  override def dmux4way(in: HBit, s: Vector[HBit]): Vector[HBit] = {
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)
    (en.homNand.fastdmux4way((in.v ++ sv).toArray).toVector grouped en.bsize).map(HBit(_)).toVector
  }

  override def dmux8way(in: HBit, s: Vector[HBit]): Vector[HBit] = {
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)
    val w = dmux(in, s(2))
    val h0 = dmux4way(w(0), s take 2)
    val h1 = dmux4way(w(1), s take 2)
    Vector(h0(0), h0(1), h0(2), h0(3), h1(0), h1(1), h1(2), h1(3))
    (en.homNand.fastdmux8way((in.v ++ sv).toArray).toVector grouped en.bsize).map(HBit(_)).toVector
  }
}

object HBitArith extends EBitArith(HBitLogic) {
  override val el = HBitLogic
}

object HBitSeq extends EBitSeq(HBitArith) {
  override val ea = HBitArith
  override def bit(s: HBit, in: HBit, load: HBit): (HBit, HBit) = (s, in, load) match {
    case (HBit(i), HBit(j), HBit(k)) => {
      val f = (ea.el.en.homNand.fastbit((i ++ j ++ k).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
      // (ea.el mux (s, in, load), s)
      (f(0), f(1))
    }
  }

  override def reg(s: Vector[HBit], in: Vector[HBit], load: HBit): (Vector[HBit], Vector[HBit]) = (s, in, load) match {
    case (_, _, HBit(k)) => {
      val so = s.map { case HBit(i) => i }.reduce(_ ++ _)
      val ino = in.map { case HBit(i) => i }.reduce(_ ++ _)
      val f = (ea.el.en.homNand.fastreg((so ++ ino ++ k).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
      val fv = (f grouped 16).toVector
      // (ea.el mux (s, in, load), s)
      (fv(0), fv(1))
    }
  }


  override def ram8(
    s: Vector[HBit], in: Vector[HBit],
    load: HBit, address: Vector[HBit]): (Vector[HBit], Vector[HBit]) = {
      // so : 128 * 16
      val so = s.map{ case HBit(i) => i }.reduce( _ ++ _ )
      val ino = in.map{ case HBit(i) => i }.reduce(_ ++ _)
      val ado = address.map{ case HBit(i) => i}.reduce(_ ++ _)
      val f = (ea.el.en.homNand.fastram8((so ++ ino ++ load.v ++ ado).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
      val fv = (f grouped 128).toVector
      (fv(0), fv(1))
    }

  override def ram64(
    s: Vector[HBit], in: Vector[HBit],
    load: HBit, address: Vector[HBit]): (Vector[HBit], Vector[HBit]) = {
      // so : 1024 * 16
      val so = s.map{ case HBit(i) => i }.reduce( _ ++ _ )
      val ino = in.map{ case HBit(i) => i }.reduce(_ ++ _)
      val ado = address.map{ case HBit(i) => i}.reduce(_ ++ _)
      val f = (ea.el.en.homNand.fastram64((so ++ ino ++ load.v ++ ado).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
      val fv = (f grouped 1024).toVector
      (fv(0), fv(1))
    }

  override def ram512(
    s: Vector[HBit], in: Vector[HBit],
    load: HBit, address: Vector[HBit]): (Vector[HBit], Vector[HBit]) = {
      // so : 8192 * 16
      val so = s.map{ case HBit(i) => i }.reduce( _ ++ _ )
      val ino = in.map{ case HBit(i) => i }.reduce(_ ++ _)
      val ado = address.map{ case HBit(i) => i}.reduce(_ ++ _)
      val f = (ea.el.en.homNand.fastram512((so ++ ino ++ load.v ++ ado).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
      val fv = (f grouped 8192).toVector
      (fv(0), fv(1))
    }

  override def rom8(s: Vector[HBit], address: Vector[HBit]): Vector[HBit] = {
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)
    val av = address.map { case HBit(i) => i }.reduce(_ ++ _)
    (ea.el.en.homNand.fastmux8way16((sv ++ av).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
  }
  override def rom64(s: Vector[HBit], address: Vector[HBit]): Vector[HBit] = {
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)
    val av = address.map { case HBit(i) => i }.reduce(_ ++ _)
    (ea.el.en.homNand.fastmux64way16((sv ++ av).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
  }
  override def rom512(s: Vector[HBit], address: Vector[HBit]): Vector[HBit] = {
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)
    val av = address.map { case HBit(i) => i }.reduce(_ ++ _)
    (ea.el.en.homNand.fastmux512way16((sv ++ av).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
  }
  override def rom4k(s: Vector[HBit], address: Vector[HBit]): Vector[HBit] = {
    val sv = s.map { case HBit(i) => i }.reduce(_ ++ _)
    val av = address.map { case HBit(i) => i }.reduce(_ ++ _)
    (ea.el.en.homNand.fastmux4kway16((sv ++ av).toArray).toVector grouped ea.el.en.bsize).map(HBit(_)).toVector
  }

}

object HBitComputer extends EBitComputer(HBitSeq) {
  override val es = HBitSeq
}
