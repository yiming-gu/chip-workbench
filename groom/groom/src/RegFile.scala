package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import groom._
import org.chipsalliance.rocketv._

object RegFileParameter {
  implicit def rwP: upickle.default.ReadWriter[RegFileParameter] = upickle.default.macroRW[RegFileParameter]
}

case class RegFileParameter(
  xLen: Int,
  paddrBits: Int,
  intIssueWidth: Int,
  memIssueWidth: Int,
  pregNum: Int,
  uopSize: Int,
  robNum: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val pregSize = log2Ceil(pregNum)
  val robSize = log2Ceil(robNum)
}

class RegFileInterface(parameter: RegFileParameter) extends Bundle {
  val intIssuePacket = Vec(parameter.intIssueWidth, Flipped(Valid(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
  val memIssuePacket = Vec(parameter.memIssueWidth, Flipped(Valid(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
}

@instantiable
class RegFileStage(val parameter: RegFileParameter) extends Module with SerializableModule[RegFileParameter] {

  @public
  val io = IO(new RegFileInterface(parameter))

}

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  val rf: Mem[UInt] = Mem(n, UInt(w.W))
  private def access(addr: UInt): UInt = rf(~addr(log2Ceil(n) - 1, 0))
  private val reads                 = collection.mutable.ArrayBuffer[(UInt, UInt)]()
  private var canRead               = true
  def read(addr: UInt)              = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(zero.B && addr === 0.U, 0.U, access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when(addr =/= 0.U) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when(addr === raddr) { rdata := data }
    }
  }
}