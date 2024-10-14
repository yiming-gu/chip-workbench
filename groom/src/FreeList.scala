import chisel3._
import chisel3.util._

case class FreeListParameter(
  renameWidth: Int,
  pregNum: Int,
) {
  val pregSize = log2Ceil(pregNum)
}

class FreeListInterface(parameter: FreeListParameter) extends Bundle {
  val allocateReq = Input(Vec(parameter.renameWidth, Bool()))
  val allocatePhyReg = Vec(parameter.renameWidth, Valid(UInt(parameter.pregSize.W)))

  val freeReq = Input(Vec(parameter.renameWidth, Bool()))
  val redirect = Input(Bool())
  val headPRedirect = Input(UInt(parameter.pregSize.W))
}

class FreeList(parameter: FreeListParameter) extends Module {

  val io = IO(new FreeListInterface(parameter))

  val freeHeadPOH = RegInit(1.U, UInt(parameter.pregNum.W))  // x0 is fixed
  val freeTailPOH = RegInit(parameter.pregNum.U, UInt(parameter.pregNum.W))

  val freeHeadP = RegInit(1.U, UInt(parameter.pregSize.W))
  val freeTailP = RegInit(parameter.pregNum.U, UInt(parameter.pregNum.W))

  freeHeadP := Mux(io.redirect, io.headPRedirect, freeHeadP + PopCount(io.allocateReq))
  freeTailP := freeTailP + PopCount(io.freeReq)

}