package HomNAND
import java.nio.file.{ Files, Paths, StandardOpenOption }

object Main {
  def main(args: Array[String]) {

    val nandcircuit: EBitNand = PBitNand
    //val progEnv: EProg = new EProg(new EBitLoader(HBitComputer))
    val progEnv: EProg = new EProg(new EBitLoader(new EBitComputer(new EBitSeq(new EBitArith(new EBitLogic(PBitNand))))))

    if (args(0) == "compile")
    {
    val pname = "examples/sample.ebc"
    val prog = EBCCompilerIntegration(pname)
    prog match {
      case Right(f) => {
        val outd = nandcircuit.exportP(f.map(nandcircuit(_)))
        val outname = "examples/sample.mnd"
        Files.write(Paths.get(outname), outd, StandardOpenOption.CREATE,StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
      }
      case _ => ()
    }
    println("compile success")
    }
    if (args(0) == "execute")
    {
    println("execute!")
    val state = progEnv.initState("examples/sample.mnd")
    val nextState = progEnv.run(state, 252)
    progEnv.writeState("examples/sample.mnd.state", nextState)
    println("finish!")
    }

    if (args(0) == "receive")
    {
      println("receive!")

      val nprogEnv: EProg = new EProg(new EBitLoader(new EBitComputer(new EBitSeq(new EBitArith(new EBitLogic(PBitNand))))))
      val byteArray = Files.readAllBytes(Paths.get("examples/sample.mnd.state"))
      val tArray = nandcircuit.importP(byteArray)
      val pArray = tArray.map((x) => nprogEnv.ld.ec.es.ea.el.en(nandcircuit.decrypt(x)))
      val r0 = nprogEnv.toInt16(pArray.slice( 16 * 16, 17 * 16))
      val r1 = nprogEnv.toInt16(pArray.slice( 17 * 16, 18 * 16))
      val r2 = nprogEnv.toInt16(pArray.slice( 18 * 16, 19 * 16))
      val r3 = nprogEnv.toInt16(pArray.slice( 19 * 16, 20 * 16))
      val sp = nprogEnv.toInt16(pArray.slice( 15 * 16, 16 * 16))
      val bp = nprogEnv.toInt16(pArray.slice( 14 * 16 ,15 * 16))
      val rt = nprogEnv.toInt16(pArray.slice(13 * 16, 14 * 16))
      val rt2 = nprogEnv.toInt16(pArray.slice(12 * 16, 13 * 16))
      val mainRet = nprogEnv.toInt16(pArray.slice(8 * 16, 9 * 16))
      val finishFlag = nprogEnv.toInt16(pArray.slice(9 * 16, 10 * 16))
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
      println("------------------------------------------")


    }

    // EncFactory.test()
  }
}
