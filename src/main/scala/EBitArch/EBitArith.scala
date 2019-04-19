package HomNAND

class EBitArith(e: EBitLogic) {
  val el: EBitLogic = e
  type T = el.T

  def zero16(a: Vector[el.T], zero: el.T): Vector[el.T] = {
    el mux16 (a, Vector.fill(16)(el.en.efalse), zero)
  }

  def neg16(a: Vector[el.T], neg: el.T): Vector[el.T] = {
    val nota = el not16 a
    el mux16 (a, nota, neg)
  }

  def halfadder(a: el.T, b: el.T): (el.T, el.T) = {
    (el xor (a, b), el and (a, b))
  }

  def fulladder(a: el.T, b: el.T, c: el.T): (el.T, el.T) = {
    val (s1, c1) = halfadder(a, b)
    val (s2, c2) = halfadder(s1, c)
    (s2, el or (c1, c2))
  }

  def add16(a: Vector[el.T], b: Vector[el.T]): Vector[el.T] = {
    val (s0, c1) = halfadder(a(0), b(0))
    val (s1, c2) = fulladder(a(1), b(1), c1)
    val (s2, c3) = fulladder(a(2), b(2), c2)
    val (s3, c4) = fulladder(a(3), b(3), c3)
    val (s4, c5) = fulladder(a(4), b(4), c4)
    val (s5, c6) = fulladder(a(5), b(5), c5)
    val (s6, c7) = fulladder(a(6), b(6), c6)
    val (s7, c8) = fulladder(a(7), b(7), c7)
    val (s8, c9) = fulladder(a(8), b(8), c8)
    val (s9, c10) = fulladder(a(9), b(9), c9)
    val (s10, c11) = fulladder(a(10), b(10), c10)
    val (s11, c12) = fulladder(a(11), b(11), c11)
    val (s12, c13) = fulladder(a(12), b(12), c12)
    val (s13, c14) = fulladder(a(13), b(13), c13)
    val (s14, c15) = fulladder(a(14), b(14), c14)
    val (s15, _) = fulladder(a(15), b(15), c15)
    Vector(s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)
  }

  def inc16(a: Vector[el.T]): Vector[el.T] = {
    add16(a, Vector.tabulate(16)((n) => if (n == 0) el.en.etrue else el.en.efalse))
  }

  def alu(
    x: Vector[el.T], y: Vector[el.T],
    zx: el.T, nx: el.T, zy: el.T, ny: el.T,
    f: el.T, no: el.T): (Vector[el.T], el.T, el.T) = {
    val xstep1 = zero16(x, zx)
    val ystep1 = zero16(y, zy)
    val nxstep1 = el not16 xstep1
    val nystep1 = el not16 ystep1
    val xstep2 = el mux16 (xstep1, nxstep1, nx)
    val ystep2 = el mux16 (ystep1, nystep1, ny)
    val fcase1 = add16(xstep2, ystep2)
    val fcase0 = el and16(xstep2, ystep2)
    val outstep1 = el mux16 (fcase0, fcase1, f)
    val noutstep1 = el not16(outstep1)
    val out = el mux16 (outstep1, noutstep1, no)
    val (out0, out1) = out splitAt 8
    val nzr = el or (el or8way (out0), el or8way (out1))
    val zr = el not nzr
    val ng = out(15)
    (out, zr, ng)
  }
}
