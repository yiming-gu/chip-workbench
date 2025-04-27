import groom._
import spire.std.int

object Elaborate extends App {
  val firtoolOptions = Array(
    "--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket",
    ).reduce(_ + "," + _),
    // "--disable-layers=Verification"
  )

  val groomParam: GroomParameter = GroomParameter(
    instructionSets = Set("rv32_i"),
    flushOnFenceI = false,
    mulUnroll = 16,
    paddrBits = 32,
    cacheBlockBytes = 32,
    dcacheNSets = 32,
    renameWidth = 3,
    decodeWidth = 3,
    pregNum = 128,
    instBytes = 4,
    robNum = 32,
    commitWidth = 3,
    intIssueWidth = 3,
    wakeUpWidth = 3,
    writeBackWidth = 3,
  )

  // FreeListMain.config(FreeListMain.FreeListParameterMain(3, 128))
  // FreeListMain.design(os.pwd / "FreeListMain.json")

  circt.stage.ChiselStage.emitSystemVerilogFile(new Groom(groomParam), args, firtoolOptions)
}
