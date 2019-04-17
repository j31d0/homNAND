package HomNAND

class EBitLogic(e: EBitNand) {
  val en: EBitNand = e
  type T = en.T
  def and(a: en.T, b: en.T): en.T = {
    val n = en nand (a, b)
    en nand (n, n)
  }
  def or(a: en.T, b: en.T): en.T = {
    val an = en nand (a, a)
    val bn = en nand (b, b)
    en nand (an, bn)
  }
  def xor(a: en.T, b: en.T): en.T = {
    val abn = en nand (a, b)
    val a2 = en nand (a, abn)
    val b2 = en nand (b, abn)
    en nand (a2, b2)
  }
  def not(a: en.T): en.T = {
    en nand (a, a)
  }
  def mux(a: en.T, b: en.T, s: en.T): en.T = {
    en nand (en nand (a, en nand (s, s)), en nand (b, s))
  }

  def dmux(i: en.T, s: en.T): Vector[en.T] = {
    val sn = en nand (s, s)
    val i1n = en nand (i, sn)
    val i2n = en nand (i, s)
    Vector(en nand (i1n, i1n), en nand (i2n, i2n))
  }

  def and16(a: Vector[en.T], b: Vector[en.T]): Vector[en.T] = {
    (a zip b).map { case (b1, b2) => and(b1, b2) }
  }

  def not16(a: Vector[en.T]): Vector[en.T] = {
    a.map(not(_))
  }

  def or16(a: Vector[en.T], b: Vector[en.T]): Vector[en.T] = {
    (a zip b).map { case (b1, b2) => or(b1, b2) }
  }

  def or8way(a: Vector[en.T]): en.T = {
    val v0 = or(a(0), a(1))
    val v1 = or(a(2), a(3))
    val v2 = or(a(4), a(5))
    val v3 = or(a(6), a(7))
    val v4 = or(v0, v1)
    val v5 = or(v2, v3)
    or(v4, v5)
  }

  def mux16(a: Vector[en.T], b: Vector[en.T], s: en.T): Vector[en.T] = {
    (a zip b).map { case (b1, b2) => mux(b1, b2, s) }
  }

  def mux4way16(
    a: Vector[en.T], b: Vector[en.T],
    c: Vector[en.T], d: Vector[en.T],
    s: Vector[en.T]): Vector[en.T] = {
    val w0 = mux16(a, b, s(0))
    val w1 = mux16(c, d, s(0))
    mux16(w0, w1, s(1))
  }

  def mux8way16(
    a: Vector[en.T], b: Vector[en.T],
    c: Vector[en.T], d: Vector[en.T],
    e: Vector[en.T], f: Vector[en.T],
    g: Vector[en.T], h: Vector[en.T],
    s: Vector[en.T]): Vector[en.T] = {
    val w0 = mux4way16(a, b, c, d, s take 2)
    val w1 = mux4way16(e, f, g, h, s take 2)
    mux16(w0, w1, s(2))
  }

  def dmux4way(in: en.T, s: Vector[en.T]): Vector[en.T] = {
    val w = dmux(in, s(1))
    val h0 = dmux(w(0), s(0))
    val h1 = dmux(w(1), s(0))
    Vector(h0(0), h0(1), h1(0), h1(1))
  }

  def dmux8way(in: en.T, s: Vector[en.T]): Vector[en.T] = {
    val w = dmux(in, s(2))
    val h0 = dmux4way(w(0), s take 2)
    val h1 = dmux4way(w(1), s take 2)
    Vector(h0(0), h0(1), h0(2), h0(3), h1(0), h1(1), h1(2), h1(3))
  }

}
