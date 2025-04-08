package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate, public}
import org.chipsalliance.rocketv._
import groom._

object ExecUnitParameter {
  implicit def rwP: upickle.default.ReadWriter[ExecUnitParameter] = upickle.default.macroRW[ExecUnitParameter]
}

case class ExecUnitParameter(
  xLen: Int,
  uopSize: Int,
  paddrBits: Int,
  pregNum: Int,
  robNum: Int,
  intIssueWidth: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val robSize = log2Ceil(robNum)
  val pregSize = log2Ceil(pregNum)
  val aluParameter = ALUParameter(xLen = xLen)
}

class Interface(parameter: ExecUnitParameter) extends Bundle {
  val execUnitReq = Vec(parameter.intIssueWidth, Flipped(ValidIO(new ExecUnitReq(parameter.xLen, parameter.uopSize))))
  val execPacket = Vec(parameter.intIssueWidth, Flipped(ValidIO(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
  val execUnitResp = Vec(parameter.intIssueWidth, ValidIO(new ExecUnitResp(parameter.xLen, parameter.robSize, parameter.pregSize)))
}

@instantiable
class ExecutionUnit(val parameter: ExecUnitParameter) extends Module with SerializableModule[ExecUnitParameter] {

  @public
  val io = IO(new Interface(parameter))

  val alu: Instance[ALU] = Instantiate(new ALU(parameter.aluParameter))
  alu.io.dw  := parameter.aluParameter.DW_32
  alu.io.fn  := io.execUnitReq(0).bits.fn
  alu.io.in2 := io.execUnitReq(0).bits.op2Data
  alu.io.in1 := io.execUnitReq(0).bits.op1Data

  io.execUnitResp(0).valid := io.execUnitReq(0).valid
  io.execUnitResp(0).bits.out := alu.io.out
  io.execUnitResp(0).bits.wxd := io.execPacket(0).bits.uop(parameter.decoderParameter.wxd)
  io.execUnitResp(0).bits.robIdx := io.execPacket(0).bits.robIdx
  io.execUnitResp(0).bits.pdst := io.execPacket(0).bits.preg.pdst
}