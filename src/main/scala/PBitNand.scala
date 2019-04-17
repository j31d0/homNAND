package HomNAND

case class PBit(v: Boolean)

object PBitNand extends EBitNand {
  type T = PBit
  val efalse: PBit = PBit(false)
  val etrue: PBit = PBit(true)
  def nand(a: PBit, b: PBit): PBit = PBit(!(a.v && b.v))
  def toByte(fvec: Vector[T]): Byte = {
    fvec.foldRight(0) {
      case (PBit(true), j) => j << 1 | 1
      case (PBit(false), j) => j << 1
    }.toByte
  }

  def fromByte(f: Byte): Vector[T] = {
    (0 until 8).map((i) => if ((f & (1 << i)) == 0) efalse else etrue).toVector
  }

  def exportP(fvec: Vector[T]): Array[Byte] = {
    (fvec grouped 8).map(toByte(_)).toArray
  }

  def importP(farr: Array[Byte]): Vector[T] = {
    farr.map((i) => fromByte(i)).foldLeft(Vector[T]()) {
      case (v1, v2) => v1 ++ v2
    }
  }
}

object PBitNandPriv extends EBitNandPriv {
  type T = PBit
  def decrypt(a: T): Boolean = a.v
  def apply(a: Boolean): T = PBit(a)
  def toByte(fvec: Vector[T]): Byte = {
    fvec.foldRight(0) {
      case (PBit(true), j) => j << 1 | 1
      case (PBit(false), j) => j << 1
    }.toByte
  }

  def fromByte(f: Byte): Vector[T] = {
    (0 until 8).map((i) => if ((f & (1 << i)) == 0) PBit(false) else PBit(true)).toVector
  }

  def exportP(fvec: Vector[T]): Array[Byte] = {
    (fvec grouped 8).map(toByte(_)).toArray
  }

  def importP(farr: Array[Byte]): Vector[T] = {
    farr.map((i) => fromByte(i)).foldLeft(Vector[T]()) {
      case (v1, v2) => v1 ++ v2
    }
  }
}
