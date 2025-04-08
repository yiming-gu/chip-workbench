package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import groom._

class IssueSlotInterface(parameter: IssueParameter) extends Bundle {
  val valid = Output(Bool())
  val wakeUpReq = Vec(parameter.wakeUpWidth, Flipped(Valid(new WakeUpReq(parameter.pregNum, parameter.delayWidth))))
  val inUop = Flipped(Valid(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  )))
  val outUop = Output(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))
  val issueUop = Output(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))
  val request = Output(Bool())
  val stillBeValid = Output(Bool())
  val grant = Input(Bool())
  val clear = Input(Bool())
}

@instantiable
class IssueSlot(val parameter: IssueParameter) extends Module with SerializableModule[IssueParameter] {

  @public
  val io = IO(new IssueSlotInterface(parameter))

  val valid = RegInit(false.B)
  val uop = Reg(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))
  val nextUop = WireInit(uop)

  when (io.inUop.valid) {
    valid := true.B
  }
  .elsewhen (io.clear) {
    valid := false.B
  }

  when (io.inUop.valid) {
    uop := io.inUop.bits
  }
  .otherwise {
    uop := nextUop
  }

  val psrc0Ready = RegInit(false.B)
  val psrc1Ready = RegInit(false.B)

  val uopMux = Mux(io.inUop.valid, io.inUop.bits, uop)
  val isPsrc0WakeUp = io.wakeUpReq.map { w =>
    w.valid && w.bits.pdst === uopMux.preg.psrc(0)
  }.reduce(_ || _)
  val isPsrc1WakeUp = io.wakeUpReq.map { w =>
    w.valid && w.bits.pdst === uopMux.preg.psrc(1)
  }.reduce(_ || _)

  when (isPsrc0WakeUp) {
    psrc0Ready := true.B
  }
  .elsewhen (io.inUop.valid) {
    psrc0Ready := !io.inUop.bits.preg.psrcBusy(0)
  }

  when (isPsrc1WakeUp) {
    psrc1Ready := true.B
  }
  .elsewhen (io.inUop.valid) {
    psrc1Ready := !io.inUop.bits.preg.psrcBusy(1)
  }

  nextUop.preg.psrcBusy(0) := !psrc0Ready
  nextUop.preg.psrcBusy(1) := !psrc1Ready

  io.valid := valid
  io.outUop := nextUop
  io.issueUop := uop
  io.request := valid && psrc0Ready && psrc1Ready
  io.stillBeValid := valid && !io.grant
}
