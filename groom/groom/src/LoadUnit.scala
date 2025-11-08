package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import org.chipsalliance.rocketv.DecoderParameter
import org.chipsalliance.t1.rtl._
import groom._

object LoadUnitParameter {
  implicit def rwP: upickle.default.ReadWriter[LoadUnitParameter] = upickle.default.macroRW[LoadUnitParameter]
}

case class LoadUnitParameter(
  xLen: Int,
  dispatchWidth: Int,
  commitWidth: Int,
  lqNum: Int,
  robNum: Int,
  paddrBits: Int,
  lregNum: Int,
  pregNum: Int,
  writeBackWidth: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val lqSize = log2Ceil(lqNum)
  val robSize = log2Ceil(robNum)
  val lregSize = log2Ceil(lregNum)
  val pregSize = log2Ceil(pregNum)
}

class LoadUnitInterface(parameter: LoadUnitParameter) extends Bundle {
  val reset = Input(Bool())
  // enq
  val enq = Flipped(DecoupledIO(Vec(parameter.dispatchWidth, new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))

  val wb = Flipped(ValidIO(new LqWb(parameter.lqSize, parameter.xLen)))

  val lqIdx = Vec(parameter.dispatchWidth, Output(UInt(parameter.lqSize.W)))
}

@instantiable
class LoadUnit(val parameter: LoadUnitParameter)
    extends Module
    with SerializableModule[LoadUnitParameter] {

  @public
  val io = IO(new LoadUnitInterface(parameter))

  val lqEntries = Reg(Vec(parameter.lqNum, new LqEntry(parameter.robSize, parameter.xLen)))
  val allocated = RegInit(VecInit(Seq.fill(parameter.lqNum)(false.B)))
  val writebacked = RegInit(VecInit(Seq.fill(parameter.lqNum)(false.B)))

  val lqDeqPOH = RegInit(1.U(parameter.lqNum.W))
  val lqDeqFlag = RegInit(false.B)
  val lqEnqPOH = RegInit(1.U(parameter.lqNum.W))
  val lqEnqFlag = RegInit(false.B)

  val lqEnqPOHShift = CircularShift(lqEnqPOH)
  val lqEnqPOHVec = VecInit.tabulate(parameter.dispatchWidth)(lqEnqPOHShift.left)
  val lqEnqPVec = VecInit(lqEnqPOHVec.map(OHToUInt(_)))

  val flush = Wire(Bool())

  when (flush) {
    for (i <- 0 until parameter.lqNum) {
      allocated(i) := false.B
    }
  }

  when (io.enq.fire) {
    for (i <- 0 until parameter.dispatchWidth) {
      val idx = lqEnqPVec(i)
      allocated(idx) := true.B
    }
  }

  when (io.wb.valid) {
    lqEntries(io.wb.bits.lqIdx).addr := io.wb.bits.addr
    writebacked(io.wb.bits.lqIdx) := io.wb.bits.dataValid
  }

  val lqEnqPOHNext = lqEnqPOHShift.left(parameter.dispatchWidth)
  when (io.enq.fire) {
    lqEnqPOH := lqEnqPOHNext
    when (lqEnqPOH(parameter.lqNum - 1,
                   parameter.lqNum - parameter.dispatchWidth).orR &&
          lqEnqPOHNext(parameter.dispatchWidth - 1, 0).orR) {
      lqEnqFlag := !lqEnqFlag
    }
  }
  .elsewhen (flush) {
    lqEnqPOH := 1.U
    lqEnqFlag := false.B
  }

  val lqDeqPOHShift = CircularShift(lqDeqPOH)
  val lqDeqPOHVec = VecInit.tabulate(parameter.commitWidth)(lqDeqPOHShift.left)

  val commitCandidate = Mux1H(lqDeqPOH, lqEntries)
  val commitAllocated = Mux1H(lqDeqPOH, allocated)
  val commitWritebacked = Mux1H(lqDeqPOH, writebacked)

  val commitValid = commitAllocated && commitWritebacked
  val allCommitted = commitValid

  val lqDeqPOHNext = lqDeqPOHShift.left(parameter.commitWidth)
  when (allCommitted) {
    lqDeqPOH := lqDeqPOHNext
    when (lqDeqPOH(parameter.lqNum - 1,
                   parameter.lqNum - parameter.commitWidth).orR &&
          lqDeqPOHNext(parameter.commitWidth - 1, 0).orR) {
      lqDeqFlag := !lqDeqFlag
    }
  }
  .elsewhen (flush) {
    lqDeqPOH := 1.U
    lqDeqFlag := false.B
  }

  val empty = lqEnqPOH === lqDeqPOH && lqEnqFlag === lqDeqFlag
  io.lqIdx := lqEnqPVec

  val willFull = lqEnqPOHVec.drop(1).map(p => p === lqDeqPOH).reduce(_ || _)
  io.enq.ready := !willFull && !flush
}
