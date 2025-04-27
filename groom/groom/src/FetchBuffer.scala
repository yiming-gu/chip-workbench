package groom.rtl.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.t1.rtl._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import org.chipsalliance.t1.rtl._
import groom._

object FetchBufferParameter {
  implicit def rwP: upickle.default.ReadWriter[FetchBufferParameter] = upickle.default.macroRW[FetchBufferParameter]
}

case class FetchBufferParameter(
  fetchWidth: Int,
  paddrBits: Int,
  decodeWidth: Int,
  fetchBufferEntries: Int,
) extends SerializableModuleParameter {

  // fetchWidth require 2^n
  val fetchPacketNum = fetchBufferEntries / decodeWidth
}

class FetchBufferReq(parameter: FetchBufferParameter) extends Bundle {
  val pc   = UInt(parameter.paddrBits.W)
  val mask = UInt(parameter.fetchWidth.W)
  val inst = Vec(parameter.fetchWidth, UInt(32.W))
}

class FetchBufferResp(parameter: FetchBufferParameter) extends Bundle {
  val fetchPacket = Vec(parameter.decodeWidth, Valid(new FetchPacket(parameter.paddrBits)))
}

class FetchBufferInterface(parameter: FetchBufferParameter) extends Bundle {
  val clear = Input(Bool())
  val enq = Flipped(DecoupledIO(new FetchBufferReq(parameter)))
  val deq = DecoupledIO(new FetchBufferResp(parameter))
}

@instantiable
class FetchBuffer(val parameter: FetchBufferParameter) extends Module with SerializableModule[FetchBufferParameter] {
  @public
  val io = IO(new FetchBufferInterface(parameter))

  val fetchBuffer = Reg(Vec(parameter.fetchBufferEntries, new FetchPacket(parameter.paddrBits)))
  val fetchBufferMatrix = Wire(Vec(parameter.fetchPacketNum, Vec(parameter.decodeWidth, new FetchPacket(parameter.paddrBits))))

  val headPOH = RegInit(1.U(parameter.fetchPacketNum.W))
  val headFlag = RegInit(false.B)
  val tailPOH = RegInit(1.U(parameter.fetchBufferEntries.W))
  val tailFlag = RegInit(false.B)

  val tailPOHShift = CircularShift(tailPOH)
  val tailPOHVec = VecInit.tabulate(parameter.fetchWidth + 1)(tailPOHShift.left)
  val tailPVec = VecInit(tailPOHVec.map(OHToUInt(_)))

  def inc(ptr: UInt) = {
    val n = ptr.getWidth
    Cat(ptr(n-2,0), ptr(n-1))
  }

  for (i <- 0 until parameter.fetchBufferEntries) {
    fetchBufferMatrix(i/parameter.decodeWidth)(i%parameter.decodeWidth) := fetchBuffer(i)
  }

  val inMask: Seq[Bool] = io.enq.bits.mask.asBools
  val inUops: Seq[FetchPacket] = io.enq.bits.inst.zipWithIndex.map { case (inst, index) =>
    val fetchPacket = Wire(new FetchPacket(parameter.paddrBits))
    fetchPacket.inst := inst
    fetchPacket.pc := io.enq.bits.pc(parameter.paddrBits-1, log2Ceil(parameter.fetchWidth)+2) ## index.asUInt(log2Ceil(parameter.fetchWidth).W) ## 0.U(2.W)
    fetchPacket
  }

  for (i <- 0 until parameter.fetchWidth) {
    when (inMask(i) && io.enq.fire) {
      fetchBuffer(tailPVec(PopCount(inMask.take(i)))) := inUops(i)
    }
  }

  val tailPOHNext = tailPOHVec(PopCount(inMask))
  when (io.enq.fire) {
    tailPOH := tailPOHNext
    when (tailPOH(parameter.fetchBufferEntries - 1,
                  parameter.fetchBufferEntries - parameter.fetchWidth).orR &&
          tailPOHNext(parameter.fetchWidth - 1, 0).orR) {
      tailFlag := !tailFlag
    }
  }

  val headPOHNext = inc(headPOH)
  when (io.deq.fire) {
    headPOH := headPOHNext
    when (headPOHNext === 1.U) {
      headFlag := !headFlag
    }
  }

  val tailPOHDeq = VecInit.tabulate(parameter.fetchPacketNum)(i => tailPOH((i+1)*parameter.decodeWidth - 1, i * parameter.decodeWidth).orR).asUInt
  val tailPOHNextDeq = VecInit.tabulate(parameter.fetchPacketNum)(i => tailPOHNext((i+1)*parameter.decodeWidth - 1, i * parameter.decodeWidth).orR).asUInt
  val full = tailPOH === tailPOHDeq && tailFlag =/= headFlag
  val willFull  = headPOH === tailPOHNextDeq
  val empty = headPOH === tailPOHDeq && headFlag === tailFlag

  io.deq.valid := !empty
  io.enq.ready := !willFull
  io.deq.bits.fetchPacket.map(_.valid := !empty)  // each output is valid
  io.deq.bits.fetchPacket.zip(Mux1H(headPOH, fetchBufferMatrix)).map { case (d, q) => d.bits := q }

  when (io.clear) {
    headPOH := 1.U
    tailPOH := 1.U
    headFlag := false.B
    tailFlag := false.B
  }
}
