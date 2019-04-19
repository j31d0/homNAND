package HomNAND

class EBitSeq(e: EBitArith) {

  val ea: EBitArith = e
  type T = ea.T

  def bit(s: ea.T, in: ea.T, load: ea.T): (ea.T, ea.T) = {
    (ea.el mux (s, in, load), s)
  }

  def reg(s: Vector[ea.T], in: Vector[ea.T], load: ea.T): (Vector[ea.T], Vector[ea.T]) = {
    val ns = (s zip in).map { case (b1, b2) => (ea.el mux (b1, b2, load)) }
    (ns, s)
  }

  def ram8(
    s: Vector[ea.T], in: Vector[ea.T],
    load: ea.T, address: Vector[ea.T]): (Vector[ea.T], Vector[ea.T]) = {
    val loads = ea.el dmux8way (load, address)
    val vs = (s grouped 16).toVector
    val (v0, o0) = reg(vs(0), in, loads(0))
    val (v1, o1) = reg(vs(1), in, loads(1))
    val (v2, o2) = reg(vs(2), in, loads(2))
    val (v3, o3) = reg(vs(3), in, loads(3))
    val (v4, o4) = reg(vs(4), in, loads(4))
    val (v5, o5) = reg(vs(5), in, loads(5))
    val (v6, o6) = reg(vs(6), in, loads(6))
    val (v7, o7) = reg(vs(7), in, loads(7))
    val ns = v0 ++ v1 ++ v2 ++ v3 ++ v4 ++ v5 ++ v6 ++ v7
    val out = ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, address)
    (ns, out)
  }

  def ram64(
    s: Vector[ea.T], in: Vector[ea.T],
    load: ea.T, address: Vector[ea.T]): (Vector[ea.T], Vector[ea.T]) = {
    val (addlow, addhigh) = address splitAt 3
    val loads = ea.el dmux8way (load, addhigh)
    val vs = (s grouped 128).toVector
    val (v0, o0) = ram8(vs(0), in, loads(0), addlow)
    val (v1, o1) = ram8(vs(1), in, loads(1), addlow)
    val (v2, o2) = ram8(vs(2), in, loads(2), addlow)
    val (v3, o3) = ram8(vs(3), in, loads(3), addlow)
    val (v4, o4) = ram8(vs(4), in, loads(4), addlow)
    val (v5, o5) = ram8(vs(5), in, loads(5), addlow)
    val (v6, o6) = ram8(vs(6), in, loads(6), addlow)
    val (v7, o7) = ram8(vs(7), in, loads(7), addlow)
    val ns = v0 ++ v1 ++ v2 ++ v3 ++ v4 ++ v5 ++ v6 ++ v7
    val out = ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, addhigh)
    (ns, out)
  }

  def ram512(
    s: Vector[ea.T], in: Vector[ea.T],
    load: ea.T, address: Vector[ea.T]): (Vector[ea.T], Vector[ea.T]) = {
    val (addlow, addhigh) = address splitAt 6
    val loads = ea.el dmux8way (load, addhigh)
    val vs = (s grouped 1024).toVector
    val (v0, o0) = ram64(vs(0), in, loads(0), addlow)
    val (v1, o1) = ram64(vs(1), in, loads(1), addlow)
    val (v2, o2) = ram64(vs(2), in, loads(2), addlow)
    val (v3, o3) = ram64(vs(3), in, loads(3), addlow)
    val (v4, o4) = ram64(vs(4), in, loads(4), addlow)
    val (v5, o5) = ram64(vs(5), in, loads(5), addlow)
    val (v6, o6) = ram64(vs(6), in, loads(6), addlow)
    val (v7, o7) = ram64(vs(7), in, loads(7), addlow)
    val ns = v0 ++ v1 ++ v2 ++ v3 ++ v4 ++ v5 ++ v6 ++ v7
    val out = ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, addhigh)
    (ns, out)
  }

  def ram4k(
    s: Vector[ea.T], in: Vector[ea.T],
    load: ea.T, address: Vector[ea.T]): (Vector[ea.T], Vector[ea.T]) = {
    val (addlow, addhigh) = address splitAt 9
    val loads = ea.el dmux8way (load, addhigh)
    val vs = (s grouped 8192).toVector
    val (v0, o0) = ram512(vs(0), in, loads(0), addlow)
    val (v1, o1) = ram512(vs(1), in, loads(1), addlow)
    val (v2, o2) = ram512(vs(2), in, loads(2), addlow)
    val (v3, o3) = ram512(vs(3), in, loads(3), addlow)
    val (v4, o4) = ram512(vs(4), in, loads(4), addlow)
    val (v5, o5) = ram512(vs(5), in, loads(5), addlow)
    val (v6, o6) = ram512(vs(6), in, loads(6), addlow)
    val (v7, o7) = ram512(vs(7), in, loads(7), addlow)
    val ns = v0 ++ v1 ++ v2 ++ v3 ++ v4 ++ v5 ++ v6 ++ v7
    val out = ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, addhigh)
    (ns, out)
  }

  def ram32k(
    s: Vector[ea.T], in: Vector[ea.T],
    load: ea.T, address: Vector[ea.T]): (Vector[ea.T], Vector[ea.T]) = {
    val (addlow, addhigh) = address splitAt 12
    val loads = ea.el dmux8way (load, addhigh)
    val vs = ((s grouped 65536).toVector zip loads).par
    val (vsr, osr) = vs.map{ case (s, l) => ram4k(s, in, l, addlow) }.unzip
    val ns = vsr.reduce(_ ++ _)
    val out = ea.el mux8way16 (osr(0), osr(1), osr(2), osr(3), osr(4), osr(5), osr(6), osr(7), addhigh)
    (ns, out)
  }

  def rom8(s: Vector[ea.T], address: Vector[ea.T]): Vector[ea.T] = {
    val vs = (s grouped 16).toVector
    val o0 = vs(0)
    val o1 = vs(1)
    val o2 = vs(2)
    val o3 = vs(3)
    val o4 = vs(4)
    val o5 = vs(5)
    val o6 = vs(6)
    val o7 = vs(7)
    ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, address)
  }

  def rom64(s: Vector[ea.T], address: Vector[ea.T]): Vector[ea.T] = {
    val (addlow, addhigh) = address splitAt 3
    val vs = (s grouped 128).toVector
    val o0 = rom8(vs(0), addlow)
    val o1 = rom8(vs(1), addlow)
    val o2 = rom8(vs(2), addlow)
    val o3 = rom8(vs(3), addlow)
    val o4 = rom8(vs(4), addlow)
    val o5 = rom8(vs(5), addlow)
    val o6 = rom8(vs(6), addlow)
    val o7 = rom8(vs(7), addlow)
    ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, addhigh)
  }

  def rom512(s: Vector[ea.T], address: Vector[ea.T]): Vector[ea.T] = {
    val (addlow, addhigh) = address splitAt 6
    val vs = (s grouped 1024).toVector
    val o0 = rom64(vs(0), addlow)
    val o1 = rom64(vs(1), addlow)
    val o2 = rom64(vs(2), addlow)
    val o3 = rom64(vs(3), addlow)
    val o4 = rom64(vs(4), addlow)
    val o5 = rom64(vs(5), addlow)
    val o6 = rom64(vs(6), addlow)
    val o7 = rom64(vs(7), addlow)
    ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, addhigh)
  }

  def rom4k(s: Vector[ea.T], address: Vector[ea.T]): Vector[ea.T] = {
    val (addlow, addhigh) = address splitAt 9
    val vs = (s grouped 8192).toVector
    val o0 = rom512(vs(0), addlow)
    val o1 = rom512(vs(1), addlow)
    val o2 = rom512(vs(2), addlow)
    val o3 = rom512(vs(3), addlow)
    val o4 = rom512(vs(4), addlow)
    val o5 = rom512(vs(5), addlow)
    val o6 = rom512(vs(6), addlow)
    val o7 = rom512(vs(7), addlow)
    ea.el mux8way16 (o0, o1, o2, o3, o4, o5, o6, o7, addhigh)
  }

  def rom32k(s: Vector[ea.T], address: Vector[ea.T]): Vector[ea.T] = {
      val (addlow, addhigh) = address splitAt 12
      val vs = (s grouped 65536).toVector.par
      val vsr = vs.map(rom4k(_, addlow))
      ea.el mux8way16 (vsr(0), vsr(1), vsr(2), vsr(3), vsr(4), vsr(5), vsr(6), vsr(7), addhigh)
  }

  def pc(s: Vector[ea.T], in: Vector[ea.T], load: ea.T, inc: ea.T, reset: ea.T): (Vector[ea.T], Vector[ea.T]) = {
    val iout = ea inc16 s
    val midout1 = ea.el mux16 (s, iout, inc)
    val midout2 = ea.el mux16 (midout1, in, load)
    val fout = ea.zero16 (midout2, reset)
    (fout, s)
  }
}
