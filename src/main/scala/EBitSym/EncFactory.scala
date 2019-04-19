package HomNAND
import scala.util.Random
import java.io._
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
    val k = isubstitution(a)
    val kb = (k zip key).map{ case (i, j) => i xor j}
    kb
  }
  def enc(da: Vector[EBitSym], key: Vector[EBitSym]) = {
    val db = (da zip key).map{ case (i, j) => i xor j}
    val dk = substitution(db)
    dk
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
    println(ex)
    val k = ex.map(_.toExpr)
    val kl = k.length
    val k2 = kl * 2
    val writer = new PrintWriter(new File("./libcircuit/HomNAND_EFastCircuit.c"))
    val templates = List("#include \"HomNAND_EFastCircuit.h\"",
      "#include <string.h>",
      "static void fastnand(jboolean in[], jboolean out[]) {")
    val templatef = List(
      "}",
      "JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastnand",
      "  (JNIEnv * env, jobject obj, jbooleanArray arr) {",
      "jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);",
      s"jboolean out[${kl}];",
      "fastnand(in, out);",
      s"jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,${kl});",
      "(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);",
      s"(*env)->SetBooleanArrayRegion(env,outp,0,${kl},(jboolean*)(&out));",
      "return outp;",
      "}",
      "JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastand",
      "  (JNIEnv * env, jobject obj, jbooleanArray arr) {",
      "jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);",
      s"jboolean out[${k2}];",
      s"jboolean out2[${kl}];",
      "fastnand(in, out);",
      s"memcpy(&out[${kl}], out, ${kl} * sizeof(jboolean));",
      "fastnand(out, out2);",
      s"jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,${kl});",
      "(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);",
      s"(*env)->SetBooleanArrayRegion(env,outp,0,${kl},(jboolean*)(&out2));",
      "return outp;",
      "}",
      "JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastor",
      "  (JNIEnv * env, jobject obj, jbooleanArray arr) {",
      "jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);",
      s"jboolean ina[${k2}];",
      s"jboolean inb[${k2}];",
      s"jboolean out[${k2}];",
      s"jboolean out2[${kl}];",
      s"memcpy(&ina[0], &in[0], ${kl} * sizeof(jboolean));",
      s"memcpy(&ina[${kl}], &in[0], ${kl} * sizeof(jboolean));",
      s"memcpy(&inb[0], &in[${kl}], ${kl} * sizeof(jboolean));",
      s"memcpy(&inb[${kl}], &in[${kl}], ${kl} * sizeof(jboolean));",
      "fastnand(ina, &out[0]);",
      s"fastnand(inb, &out[${kl}]);",
      "fastnand(out, out2);",
      s"jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,${kl});",
      "(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);",
      s"(*env)->SetBooleanArrayRegion(env,outp,0,${kl},(jboolean*)(&out2));",
      "return outp;",
      "}",
      "JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastxor",
      "  (JNIEnv * env, jobject obj, jbooleanArray arr) {",
      "jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);",
      s"jboolean ab1[${k2}];",
      s"jboolean ab2[${k2}];",
      s"jboolean out[${kl}];",
      "fastnand(in, ab1);",
      s"memcpy(&ab1[${kl}], &in[0], ${kl} * sizeof(jboolean));",
      "fastnand(ab1, &ab2[0]);",
      s"memcpy(&ab1[${kl}], &in[${kl}], ${kl} * sizeof(jboolean));",
      s"fastnand(ab1, &ab2[${kl}]);",
      "fastnand(ab2, out);",
      s"jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,${kl});",
      "(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);",
      s"(*env)->SetBooleanArrayRegion(env,outp,0,${kl},(jboolean*)(&out));",
      "return outp;",
      "}",
      "JNIEXPORT jbooleanArray JNICALL Java_HomNAND_EFastCircuit_fastnot",
      "  (JNIEnv * env, jobject obj, jbooleanArray arr) {",
      "jboolean *in = (*env)->GetBooleanArrayElements(env, arr, 0);",
      s"jboolean a1[${k2}];",
      s"jboolean out[${kl}];",
      s"memcpy(&a1[0], &in[0], ${kl} * sizeof(jboolean));",
      s"memcpy(&a1[${kl}], &in[0], ${kl} * sizeof(jboolean));",
      "fastnand(a1, out);",
      s"jintArray outp=(jbooleanArray)(*env)->NewBooleanArray(env,${kl});",
      "(*env)->ReleaseBooleanArrayElements(env, arr, in, 0);",
      s"(*env)->SetBooleanArrayRegion(env,outp,0,${kl},(jboolean*)(&out));",
      "return outp;",
      "}"
    )
    templates.foreach((i) => writer.println(i))
    (0 until k.length).foreach((i) => writer.println(s"out[$i] = ${k(i)};"))
    templatef.foreach((i) => writer.println(i))
    writer.close()
    "make -C libcircuit".!
    println(System.mapLibraryName("EFastCircuit"))
    System.loadLibrary("EFastCircuit")
    new EFastCircuit
  }
}

case class HBit(v: Vector[Boolean])

object HBitNand extends EBitNand {
  type T = HBit
  val key: Short = 0xfe92.toShort
  val rseed = Random
  val homNand = EncFactory.prepare(EncFactory.genHNand(EncFactory.genShort(key)))
  val efalse: HBit = HBit(EncFactory.enc(EncFactory.genShort(31322.toShort), EncFactory.genShort(key)).map((f) => f.eval(Vector())))
  val etrue: HBit = HBit(EncFactory.enc(EncFactory.genShort(26585.toShort), EncFactory.genShort(key)).map((f) => f.eval(Vector())))
  def apply(a: Boolean): T = HBit(EncFactory.enc(Vector.tabulate(16)((n) => SymLocBit(n)), EncFactory.genShort(key)).map((f) => f.eval(a +: Vector.tabulate(15)((n) => rseed.nextBoolean))))
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
  override def mux(a: HBit, b: HBit, s: HBit): HBit = {
    en nand (en nand (a, en nand (s, s)), en nand (b, s))
  }
}
