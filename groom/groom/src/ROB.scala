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
  xLen: Int,
  dispatchWidth: Int,
  commitWidth: Int,
  robNum: Int,
  paddrBits: Int,
  lregNum: Int,
  pregNum: Int,
  writeBackWidth: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val robSize = log2Ceil(robNum)
  val lregSize = log2Ceil(lregNum)
  val pregSize = log2Ceil(pregNum)
}

class ROBInterface(parameter: ROBParameter) extends Bundle {
  val reset = Input(Bool())
  // enq
  val enq = Flipped(DecoupledIO(Vec(parameter.dispatchWidth, new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
  val robIdx = Vec(parameter.dispatchWidth, Output(UInt(parameter.robSize.W)))
  // writeback
  val wb = Vec(parameter.writeBackWidth, Flipped(ValidIO(new RobWb(parameter.xLen, parameter.robSize))))
  // commit
  val commit = Output(new RobCommit(
    parameter.commitWidth,
    parameter.lregSize,
    parameter.pregSize
  ))
  val flush = ValidIO(new Redirect(parameter.xLen))
}

@instantiable
class ROB(val parameter: ROBParameter)
    extends Module
    with SerializableModule[ROBParameter]
    with ImplicitReset {

  override protected def implicitReset: Reset = io.reset

  @public
  val io = IO(new ROBInterface(parameter))

  val robEntries = Reg(Vec(parameter.robNum, new RobEntry(parameter.lregSize, parameter.pregSize, parameter.xLen)))

  // val robBanks = VecInit((0 until parameter.commitWidth).map(i => VecInit(robEntries.zipWithIndex.filter(_._2 % parameter.commitWidth == i).map(_._1))))

  val robDeqPOH = RegInit(1.U(parameter.robNum.W))
  val robDeqFlag = RegInit(false.B)
  val robEnqPOH = RegInit(1.U(parameter.robNum.W))
  val robEnqFlag = RegInit(false.B)

  val robEnqPOHShift = CircularShift(robEnqPOH)
  val robEnqPOHVec = VecInit.tabulate(parameter.dispatchWidth)(robEnqPOHShift.left)
  val robEnqPVec = VecInit(robEnqPOHVec.map(OHToUInt(_)))

  val flush = Wire(Bool())

  when (io.reset.asBool) {
    for (i <- 0 until parameter.robNum) {
      robEntries(i).valid := false.B
      robEntries(i).writebacked := false.B
      robEntries(i).needFlush := false.B
    }
  }
  .elsewhen (flush) {
    for (i <- 0 until parameter.robNum) {
      robEntries(i).valid := false.B
      robEntries(i).writebacked := false.B
      robEntries(i).needFlush := false.B
    }
  }

  when (io.enq.fire) {
    for (i <- 0 until parameter.dispatchWidth) {
      val idx = robEnqPVec(i)
      robConnectEnq(robEntries(idx), io.enq.bits(i), parameter.decoderParameter)
    }
  }

  io.wb.foreach { wb =>
    when (wb.valid) {
      robEntries(wb.bits.robIdx).writebacked := true.B
      robEntries(wb.bits.robIdx).needFlush := wb.bits.needFlush
      robEntries(wb.bits.robIdx).dnpc := wb.bits.dnpc
    }
  }

  val robEnqPOHNext = robEnqPOHShift.left(parameter.dispatchWidth)
  when (io.enq.fire) {
    robEnqPOH := robEnqPOHNext
    when (robEnqPOH(parameter.robNum - 1,
                  parameter.robNum - parameter.dispatchWidth).orR &&
          robEnqPOHNext(parameter.dispatchWidth - 1, 0).orR) {
      robEnqFlag := !robEnqFlag
    }
  }
  .elsewhen (flush) {
    robEnqPOH := 1.U
    robEnqFlag := false.B
  }

  val robDeqPOHShift = CircularShift(robDeqPOH)
  val robDeqPOHVec = VecInit.tabulate(parameter.commitWidth)(robDeqPOHShift.left)

  val commitCandidates = VecInit(robDeqPOHVec.map(sel => Mux1H(sel, robEntries)))
  val commitPOH = RegInit(1.U(parameter.commitWidth.W))
  val commitPOHShift = CircularShift(commitPOH)
  val commitPOHVec = VecInit.tabulate(parameter.commitWidth)(commitPOHShift.left)

  val hasCommitted = RegInit(VecInit.fill(parameter.commitWidth)(false.B))
  val commitValid = VecInit(Seq.tabulate(parameter.commitWidth)(i => commitCandidates.map(c => c.valid && c.writebacked).take(i+1).reduce(_ && _)).zip(hasCommitted).map(c => c._1 && !c._2))
  val allCommitted = io.commit.isCommit && io.commit.commitValid.last
  val allowOnlyOneCommit = VecInit(commitCandidates.map(x => x.valid && x.writebacked && x.needFlush)).asUInt.orR

  when (allCommitted || flush) {
    hasCommitted := 0.U.asTypeOf(hasCommitted)
  }
  .elsewhen (io.commit.isCommit) {
    for (i <- 0 until parameter.commitWidth) {
      hasCommitted(i) := io.commit.commitValid(i) || hasCommitted(i)
    }
  }

  when (allCommitted || flush) {
    commitPOH := 1.U
  }
  .otherwise {
    commitPOH := commitPOHVec(PopCount(io.commit.commitValid))
  }

  flush := commitCandidates(OHToUInt(commitPOH)).needFlush

  val robDeqPOHNext = robDeqPOHShift.left(parameter.commitWidth)
  when (allCommitted) {
    robDeqPOH := robDeqPOHNext
    when (robDeqPOH(parameter.robNum - 1,
                    parameter.robNum - parameter.commitWidth).orR &&
          robDeqPOHNext(parameter.commitWidth - 1, 0).orR) {
      robDeqFlag := !robDeqFlag
    }
  }
  .elsewhen (flush) {
    robDeqPOH := 1.U
    robDeqFlag := false.B
  }

  val empty = robEnqPOH === robDeqPOH && robEnqFlag === robDeqFlag
  io.robIdx := robEnqPVec
  io.commit.isCommit := true.B
  io.commit.commitValid := Mux(allowOnlyOneCommit, VecInit(commitPOH.asBools.zip(commitValid).map(c => c._1 && c._2)), commitValid).map(_ && !empty)
  io.commit.isWalk := false.B
  io.commit.walkValid := Seq.fill(parameter.commitWidth)(false.B)
  io.commit.wxd := commitCandidates.map(_.wxd)
  io.commit.ldst := commitCandidates.map(_.ldst)
  io.commit.pdst := commitCandidates.map(_.pdst)
  io.commit.pc := commitCandidates.map(_.pc)
  dontTouch(io.commit.pc)

  val willFull = robEnqPOHVec.drop(1).map(p => p === robDeqPOH).reduce(_ || _)
  io.enq.ready := !willFull && !flush
  io.flush.valid := RegNext(flush)
  io.flush.bits.pc := RegNext(commitCandidates(OHToUInt(commitPOH)).dnpc)
}
