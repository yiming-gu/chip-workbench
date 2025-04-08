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
  val execUnitReq = Vec(parameter.intIssueWidth, ValidIO(new ExecUnitReq(parameter.xLen, parameter.uopSize)))
  val execUnitResp = Vec(parameter.intIssueWidth, Flipped(ValidIO(new ExecUnitResp(parameter.xLen, parameter.robSize, parameter.pregSize))))
}

@instantiable
class RegFileStage(val parameter: RegFileParameter) extends Module with SerializableModule[RegFileParameter] {

  @public
  val io = IO(new RegFileInterface(parameter))

  val rf = new RegFile(parameter.pregNum, parameter.xLen)

  val execUnitValid = RegInit(VecInit.fill(parameter.intIssueWidth)(false.B))
  val execUnitReq = Reg(Vec(parameter.intIssueWidth, new ExecUnitReq(parameter.xLen, parameter.uopSize)))

  execUnitValid.zip(io.intIssuePacket).map { case (v, p) =>
    v := p.valid
  }
  execUnitReq.zip(io.intIssuePacket).map { case (r, p) =>
    r.op1Data := rf.read(p.bits.preg.psrc(0))
    r.op2Data := rf.read(p.bits.preg.psrc(1))
    r.fn := p.bits.uop(parameter.decoderParameter.aluFn)
  }

  io.execUnitReq.zip(execUnitValid).zip(execUnitReq).map { case ((ior, v), r) =>
    ior.valid := v
    ior.bits := r
  }

  io.execUnitResp.map { r =>
    when (r.valid && r.bits.wxd) {
      rf.write(r.bits.pdst, r.bits.out)
    }
  }
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