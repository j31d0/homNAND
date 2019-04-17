package HomNAND

import java.nio.file.{Files, Paths, StandardOpenOption}

class EProg(e: EBitNand) {
  val ld: EBitLoader = new EBitLoader(e)
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

  def toInt16(a: Vector[ld.T]): Short = {
    val f = ld.ec.es.ea.el.en exportP a
    ((f(0) & 0xff) | (f(1) & 0xff) << 8).toShort
  }
  def run(s: State, c: Int): State = if (c == 0) s else s match {
    case (rom, ram, a, d, pc, valueM) => {
      val (nrom, nram, na, nd, npc, nvalueM) = ld.ec computer (rom, ram, a, d, pc, valueM)
      val sp = toInt16(nram.slice( 15 * 16, 16 * 16))
      val bp = toInt16(nram.slice( 14 * 16 ,15 * 16))
      val rt = toInt16(nram.slice(13 * 16, 14 * 16))
      val rt2 = toInt16(nram.slice(12 * 16, 13 * 16))
      val mainRet = toInt16(nram.slice(8 * 16, 9 * 16))
      val stack = if (sp < bp && (sp & 0x7000) != 0 && (bp & 0x7000) != 0) ram.slice( sp * 16, bp * 16 ).grouped(16).map(toInt16(_)).toList else List()
      println(sp, bp, mainRet, stack, toInt16(npc))
      run((nrom, nram, na, nd, npc, nvalueM), c - 1)
    }
  }

  def writeState(fname: String, s: State): Unit = s match {
    case (rom, ram, a, d, pc, valueM) =>
    Files.write(Paths.get(fname), ld.ec.es.ea.el.en exportP ram, StandardOpenOption.CREATE);
  }
}
