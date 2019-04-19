package HomNAND
import java.nio.file.{Files, Paths}


class EBitLoader(e: EBitComputer) {
  val ec: EBitComputer = e
  type T = ec.T
  def apply(fname: String): (Vector[T], Vector[T]) = {
    val byteArray = Files.readAllBytes(Paths.get(fname))
    val memiter = ((ec.es.ea.el.en importP byteArray) grouped 524288).toVector
    (memiter(0), memiter(1))
  }


}
