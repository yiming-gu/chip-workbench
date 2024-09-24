package groom.frontend

import chisel3._
import chisel3.util._
import scala.annotation.meta.param

case class FetchBufferParameter(
  fetchWidth: Int,
  instBytes: Int,
  paddrBits: Int,
  decodeWidth: Int,
  fetchBufferEntries: Int,
) {
  val fetchPacketNum = fetchBufferEntries / decodeWidth
}

class FetchPacket(parameter: FetchBufferParameter) extends Bundle {
  val inst: UInt = UInt((parameter.instBytes*8).W)
  val pc:   UInt = UInt(parameter.paddrBits.W)
}

class FetchBufferReq(parameter: FetchBufferParameter) extends Bundle {
  val pc   = UInt(parameter.paddrBits.W)
  val mask = UInt(parameter.fetchWidth.W)
  val inst = Vec(parameter.fetchWidth, UInt((parameter.instBytes*8).W))
}

class FetchBufferResp(parameter: FetchBufferParameter) extends Bundle {
  val uops = Vec(parameter.decodeWidth, Valid(new FetchPacket(parameter)))
}

class FetchBufferInterface(parameter: FetchBufferParameter) extends Bundle {
  val clear = Input(Bool())
  val req  = Flipped(DecoupledIO(new FetchBufferReq(parameter)))
  val resp = DecoupledIO(new FetchBufferResp(parameter))
}

class FetchBuffer(parameter: FetchBufferParameter) extends Module {
  val io = IO(new FetchBufferInterface(parameter))

  val fetchBuffer = Reg(Vec(parameter.fetchBufferEntries, new FetchPacket(parameter)))
  val fetchBufferMatrix = Wire(Vec(parameter.fetchPacketNum, Vec(parameter.decodeWidth, new FetchPacket(parameter))))

  val head1H = RegInit(1.U(parameter.fetchPacketNum.W))
  val tail1H = RegInit(1.U(parameter.fetchBufferEntries.W))

  def inc(ptr: UInt): UInt = {
    val n = ptr.getWidth
    Cat(ptr(n-2,0), ptr(n-1))
  }

  // for (i <- 0 until parameter.fetchBufferEntries) {
  //   fetchBufferMatrix(i/parameter.decodeWidth)(i%parameter.decodeWidth) := fetchBuffer(i)
  // }

  val inMask = Wire(Vec(parameter.fetchWidth, Bool()))
  val inUops = Wire(Vec(parameter.fetchWidth, new FetchPacket(parameter)))

  inMask.zipWithIndex.map {case (mask, index) =>
    mask := io.req.bits.mask(index)
  }

  inUops.zipWithIndex.map {case (uop, index) =>
    uop.inst := io.req.bits.inst(index)
    uop.pc   := io.req.bits.pc(parameter.paddrBits-1, log2Ceil(parameter.fetchWidth)) ## index.asUInt(log2Ceil(parameter.fetchWidth).W)
  }

  fetchBufferMatrix.zipWithIndex.map {case (fetchOne, index) => fetchOne := fetchBuffer(index)}

  io.resp.bits.uops.zip(Mux1H(head1H, fetchBufferMatrix)).map {case (d, q) => d.bits := q}
}
