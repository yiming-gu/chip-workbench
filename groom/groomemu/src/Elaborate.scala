import groom.rtl.backend._

object Elaborate extends App {
  val firtoolOptions = Array("--lowering-options=" + List(
    // make yosys happy
    // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
    "disallowLocalVariables",
    "disallowPackedArrays",
    "locationInfoStyle=wrapInAtSquareBracket"
  ).reduce(_ + "," + _))

  val groomParam: GroomParameter = GroomParameter(
    instructionSets = Set("rv32_i"),
    flushOnFenceI = false,
    mulUnroll = 16,
    paddrBits = 32,
    cacheBlockBytes = 32,
    dcacheNSets = 32,
    renameWidth = 3,
    pregNum = 128,
  )

  // FreeListMain.config(FreeListMain.FreeListParameterMain(3, 128))
  // FreeListMain.design(os.pwd / "FreeListMain.json")

  circt.stage.ChiselStage.emitSystemVerilogFile(new Groom(groomParam), args, firtoolOptions)
}
