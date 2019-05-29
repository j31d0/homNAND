package HomNAND

import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.annotation.tailrec

class EProg(e: EBitLoader) {
  val ld: EBitLoader = e
  type State = (
    Vector[ld.T],
    Vector[ld.T],
    Vector[ld.T],
    Vector[ld.T],
    Vector[ld.T],
    Vector[ld.T])
  def initState(fname: String): State = {
      val (rom, ram): (Vector[ld.T], Vector[ld.T]) = ld(fname)
      val a = Vector.fill(16)(ld.ec.es.ea.el.en.efalse)
      val d = Vector.fill(16)(ld.ec.es.ea.el.en.efalse)
      val pc = Vector.fill(16)(ld.ec.es.ea.el.en.efalse)
      val valueM = Vector.fill(16)(ld.ec.es.ea.el.en.efalse)
      (rom, ram, a, d, pc, valueM)
  }

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
        sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
}
  def toInt16(a: Vector[ld.T]): String = {
    convertBytesToHex(ld.ec.es.ea.el.en exportP a)
  }
  @tailrec
  final def run(s: State, c: Int): State = if (c == 0) s else s match {
    case (rom, ram, a, d, pc, valueM) => {
      val (nrom, nram, na, nd, npc, nvalueM) = ld.ec computer (rom, ram, a, d, pc, valueM)
      val r0 = toInt16(nram.slice( 16 * 16, 17 * 16))
      val r1 = toInt16(nram.slice( 17 * 16, 18 * 16))
      val r2 = toInt16(nram.slice( 18 * 16, 19 * 16))
      val r3 = toInt16(nram.slice( 19 * 16, 20 * 16))
      val sp = toInt16(nram.slice( 15 * 16, 16 * 16))
      val bp = toInt16(nram.slice( 14 * 16 ,15 * 16))
      val rt = toInt16(nram.slice(13 * 16, 14 * 16))
      val rt2 = toInt16(nram.slice(12 * 16, 13 * 16))
      val mainRet = toInt16(nram.slice(8 * 16, 9 * 16))
      val finishFlag = toInt16(nram.slice(9 * 16, 10 * 16))
      // val stack = if (sp < bp && (sp & 0x7000) != 0 && (bp & 0x7000) != 0) ram.slice( sp * 16, bp * 16 ).grouped(16).map(toInt16(_)).toList else List()
      println("------------------------------------------")
      println(s"sp         : $sp")
      println(s"bp         : $bp")
      println(s"r0         : $r0")
      println(s"r1         : $r1")
      println(s"r2         : $r2")
      println(s"r3         : $r3")
      println(s"finishFlag : $finishFlag")
      println(s"retval     : $mainRet")
      println(s"pc         : ${toInt16(npc)}")
      println("------------------------------------------")
      run((nrom, nram, na, nd, npc, nvalueM), c - 1)
    }
  }

  def writeState(fname: String, s: State): Unit = s match {
    case (rom, ram, a, d, pc, valueM) =>
    Files.write(Paths.get(fname), ld.ec.es.ea.el.en exportP ram, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
  }
}
