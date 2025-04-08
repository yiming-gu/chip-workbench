package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import org.chipsalliance.t1.rtl._

object FreeListParameter {
  implicit def rwP: upickle.default.ReadWriter[FreeListParameter] = upickle.default.macroRW[FreeListParameter]
}

case class FreeListParameter(
  renameWidth: Int,
  commitWidth: Int,
  lregNum: Int,
  pregNum: Int,
  freelistNum: Int,
) extends SerializableModuleParameter {
  val pregSize = log2Ceil(pregNum)
}

class FreeListInterface(parameter: FreeListParameter) extends Bundle {
  val redirect = Input(Bool())
  val allocateReq = Input(Vec(parameter.renameWidth, Bool()))
  val allocatePhyReg = Output(Vec(parameter.renameWidth, UInt(parameter.pregSize.W)))

  val freeReq = Input(Vec(parameter.commitWidth, Bool()))
  val freePhyReg = Input(Vec(parameter.commitWidth, UInt(parameter.pregSize.W)))
}

@instantiable
class FreeList(val parameter: FreeListParameter) extends Module with SerializableModule[FreeListParameter] {

  @public
  val io = IO(new FreeListInterface(parameter))

  val freeList = RegInit(VecInit(Seq.tabulate(parameter.freelistNum)(i => (i+parameter.lregNum).U(parameter.pregSize.W))))

  val freeHeadPOH = RegInit(1.U(parameter.pregNum.W))
  val freeTailPOH = RegInit(1.U(parameter.pregNum.W))

  // val freeHeadP = RegInit(0.U(parameter.pregSize.W))
  // val freeTailP = RegInit(0.U(parameter.pregSize.W))

  // freeHeadP := Mux(io.redirect, io.headPRedirect, freeHeadP + PopCount(io.allocateReq))
  // freeTailP := freeTailP + PopCount(io.freeReq)

  val freeTailPOHShift = CircularShift(freeTailPOH)
  val freeTailPOHVec = VecInit.tabulate(parameter.commitWidth + 1)(freeTailPOHShift.left)
  val freeTailPVec = VecInit(freeTailPOHVec.map(OHToUInt(_)))

  for (i <- 0 until parameter.commitWidth) {
    freeList(freeTailPVec(PopCount(io.freeReq.take(i)))) := io.freePhyReg(i)
  }

  freeTailPOH := freeTailPOHVec(PopCount(io.freeReq))

  val freeHeadPOHShift = CircularShift(freeHeadPOH)
  val freeHeadPOHVec = VecInit.tabulate(parameter.renameWidth + 1)(freeHeadPOHShift.left)

  val phyRegCandidates = VecInit(freeHeadPOHVec.map(sel => Mux1H(sel, freeList)))
  for (i <- 0 until parameter.renameWidth) {
    io.allocatePhyReg(i) := phyRegCandidates(PopCount(io.allocateReq.take(i)))
  }

  freeHeadPOH := freeHeadPOHVec(PopCount(io.allocateReq))
}
