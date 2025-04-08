package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import org.chipsalliance.rocketv.DecoderParameter
import org.chipsalliance.t1.rtl._
import groom._

object ROBParameter {
  implicit def rwP: upickle.default.ReadWriter[ROBParameter] = upickle.default.macroRW[ROBParameter]
}

case class ROBParameter(
  dispatchWidth: Int,
  commitWidth: Int,
  robNum: Int,
  paddrBits: Int,
  lregNum: Int,
  pregNum: Int,
  renameWidth: Int,
  writebackWidth: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val robSize = log2Ceil(robNum)
  val lregSize = log2Ceil(lregNum)
  val pregSize = log2Ceil(pregNum)
}

class ROBInterface(parameter: ROBParameter) extends Bundle {
  // enq
  val enq = Vec(parameter.renameWidth, Flipped(ValidIO(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
  val robIdx = Vec(parameter.renameWidth, Output(UInt(parameter.robSize.W)))
  // writeback
  val wb = Vec(parameter.writebackWidth, Flipped(ValidIO(UInt(parameter.robSize.W))))
  // commit
  val commit = Vec(parameter.commitWidth, ValidIO(new RobCommit(
    parameter.lregSize,
    parameter.pregSize
  )))
}

@instantiable
class ROB(val parameter: ROBParameter) extends Module with SerializableModule[ROBParameter] {

  @public
  val io = IO(new ROBInterface(parameter))

  val robValid     = RegInit(VecInit.fill(parameter.robNum)(false.B))
  val robComplete  = Reg(Vec(parameter.robNum, Bool()))
  // val robException = Reg(Vec(parameter.robNum, Bool()))
  val robUop       = Reg(Vec(parameter.robNum, new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  )))

  val robHeadPOH = RegInit(1.U(parameter.robNum.W))
  val robTailPOH = RegInit(1.U(parameter.robNum.W))

  val enqValid = io.enq.map(_.valid)

  val robHeadPOHShift = CircularShift(robHeadPOH)
  val robHeadPOHVec = VecInit.tabulate(parameter.renameWidth + 1)(robHeadPOHShift.left)
  val robHeadPVec = VecInit(robHeadPOHVec.map(OHToUInt(_)))

  for (i <- 0 until parameter.renameWidth) {
    robValid(robHeadPVec(PopCount(enqValid.take(i)))) := true.B
    robComplete(robHeadPVec(PopCount(enqValid.take(i)))) := false.B
    robUop(robHeadPVec(PopCount(enqValid.take(i)))) := io.enq(i).bits
  }

  io.wb.foreach { wb =>
    when (wb.valid) {
      robComplete(wb.bits) := true.B
    }
  }

  robHeadPOH := robHeadPOHVec(PopCount(enqValid))
  for (i <- 0 until parameter.renameWidth) {
    io.robIdx(i) := robHeadPVec(PopCount(enqValid.take(i)))
  }

  val robTailPOHShift = CircularShift(robTailPOH)
  val robTailPOHVec = VecInit.tabulate(parameter.commitWidth + 1)(robTailPOHShift.left)

  val complete = VecInit(robTailPOHVec.map(sel => Mux1H(sel, robComplete)))
  val uop      = VecInit(robTailPOHVec.map(sel => Mux1H(sel, robUop)))
  val commitValid = complete.reduce(_ && _)

  when (commitValid) {
    robTailPOH := robTailPOHVec(PopCount(complete))
  }

  io.commit.zip(uop).foreach { case (c, u) =>
    c.valid := commitValid
    c.bits.wxd := u.uop(parameter.decoderParameter.wxd)
    c.bits.ldst := u.fetchPacket.inst(11, 7)
    c.bits.pdst := u.preg.pdst
  }
}
