package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate, public}
import org.chipsalliance.rocketv._
import groom._

object ExeUnitParameter {
  implicit def rwP: upickle.default.ReadWriter[ExeUnitParameter] = upickle.default.macroRW[ExeUnitParameter]
}

case class ExeUnitParameter(
  xLen: Int,
  robNum: Int,
  pregNum: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val robSize = log2Ceil(robNum)
  val pregSize = log2Ceil(pregNum)
  val aluParameter = ALUParameter(xLen = xLen)
}

class ExeUnitInterface(parameter: ExeUnitParameter) extends Bundle {
  val in = Flipped(ValidIO(new ExuInput(
    parameter.xLen,
    parameter.pregSize,
    parameter.robSize,
    parameter.decoderParameter
  )))
  val out = ValidIO(new ExuOutput(
    parameter.xLen,
    parameter.robSize,
    parameter.pregSize
  ))
}

@instantiable
class ExeUnit(val parameter: ExeUnitParameter) extends Module with SerializableModule[ExeUnitParameter] {

  @public
  val io = IO(new ExeUnitInterface(parameter))

  def A1_RS1 = 1.U(2.W)
  def A1_PC  = 2.U(2.W)

  def A2_ZERO = 0.U(2.W)
  def A2_SIZE = 1.U(2.W)
  def A2_RS2  = 2.U(2.W)
  def A2_IMM  = 3.U(2.W)

  val op1: SInt = MuxLookup(io.in.bits.selAlu1, 0.S)(
    Seq(A1_RS1 -> io.in.bits.rsrc(0).asSInt, A1_PC -> io.in.bits.pc.asSInt)
  )

  val op2: SInt = MuxLookup(io.in.bits.selAlu2, 0.S)(
    Seq(A2_RS2 -> io.in.bits.rsrc(1).asSInt, A2_IMM -> io.in.bits.imm.asSInt, A2_SIZE -> 4.S)
  )

  val alu: Instance[ALU] = Instantiate(new ALU(parameter.aluParameter))
  alu.io.dw := io.in.bits.dw
  alu.io.fn := io.in.bits.fn
  alu.io.in1 := op1.asUInt
  alu.io.in2 := op2.asUInt
  io.out.valid := io.in.valid
  io.out.bits.data := alu.io.out
  io.out.bits.robIdx := io.in.bits.robIdx
  io.out.bits.wxd := io.in.bits.wxd
  io.out.bits.pdst := io.in.bits.pdst
}
