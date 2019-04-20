package HomNAND
import java.nio.file.{ Files, Paths, StandardOpenOption }

object Main {
  def main(args: Array[String]) {


    val pname = "examples/sample.ebc"
    val prog = EBCCompilerIntegration(pname)
    prog match {
      case Right(f) => {
        val outd = HBitNand.exportP(f.map(HBitNand(_)))
        val outname = "examples/sample.mnd"
        Files.write(Paths.get(outname), outd, StandardOpenOption.CREATE)
      }
      case _ => ()
    }
    println("sample computer")
    val progEnv: EProg = new EProg(new EBitLoader(HBitComputer))
    val state = progEnv.initState("examples/sample.mnd")
    val nextState = progEnv.run(state, 252)
    progEnv.writeState("examples/sample.mnd.state", nextState)
    print("finish!")

    // EncFactory.test()
  }
}
