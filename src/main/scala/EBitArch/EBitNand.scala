package HomNAND

trait EBitNand {
  type T
  val efalse: T
  val etrue: T
  def nand(a: T, b: T): T
  def importP(farr: Array[Byte]): Vector[T]
  def exportP(fvec: Vector[T]): Array[Byte]
  def decrypt(a: T): Boolean
  def apply(f: Boolean): T
}

