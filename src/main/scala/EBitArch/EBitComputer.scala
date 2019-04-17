package HomNAND

class EBitComputer(e: EBitNand) {
  val es: EBitSeq = new EBitSeq(e)
  type T = es.T

  def cpu(
    a: Vector[es.T], d: Vector[es.T], pc: Vector[es.T], inM: Vector[es.T],
    inst: Vector[es.T]
  ): (Vector[es.T], Vector[es.T], Vector[es.T], es.T, Vector[es.T], Vector[es.T]) = {

    val aluarg2 = es.ea.el mux16 (a, inM, inst(12))
    val (alures, zr, ng) = es.ea alu (d, aluarg2, inst(11), inst(10), inst(9), inst(8), inst(7), inst(6))
    val outM = alures

    val nacandid = es.ea.el mux16 (inst, alures, inst(15))
    val n15 = es.ea.el not inst(15)
    val aload = es.ea.el or (n15, inst(5))
    val (na, _) = es reg (a, nacandid, aload)
    val addressM = na take 15

    val dload = es.ea.el and (inst(15), inst(4))
    val (nd, _) = es reg (d, alures, dload)

    val b1 = es.ea.el or (zr, ng)
    val nb1 = es.ea.el not b1
    val b2 = es.ea.el and (inst(0), nb1)
    val b3 = es.ea.el and (inst(1), zr)
    val b4 = es.ea.el and (inst(2), ng)
    val b5 = es.ea.el or (b2, b3)
    val b6 = es.ea.el or (b5, b4)
    val jpc = es.ea.el and (inst(15), b6)

    val (npc, _) = es pc (pc, a, jpc, es.ea.el.en.etrue, es.ea.el.en.efalse)

    val writeM = es.ea.el and (inst(15), inst(3))
    (na, nd, outM, writeM, addressM, npc)

  }

  def computer(
    rom: Vector[es.T],
    ram: Vector[es.T],
    a: Vector[es.T],
    d: Vector[es.T],
    pc: Vector[es.T],
    valueM: Vector[es.T]): (Vector[es.T], Vector[es.T], Vector[es.T], Vector[es.T], Vector[es.T], Vector[es.T])= {
    val inst = es rom32k (rom, pc take 15)
    val (na, nd, outM, writeM, addressM, npc) = cpu(a, d, pc, valueM, inst)
    val (nram, nvalueM) = es ram32k (ram, outM, writeM, addressM)
    (rom, nram, na, nd, npc, nvalueM)
  }


}
