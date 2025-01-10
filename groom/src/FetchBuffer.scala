package groom.rtl.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.t1.rtl._

case class FetchBufferParameter(
  fetchWidth: Int,
  instBytes: Int,
  paddrBits: Int,
  decodeWidth: Int,
  fetchBufferEntries: Int,
) {

  // fetchWidth require 2^n
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
  val enq = Flipped(DecoupledIO(new FetchBufferReq(parameter)))
  val deq = DecoupledIO(new FetchBufferResp(parameter))
}

class FetchBuffer(parameter: FetchBufferParameter) extends Module {
  val io = IO(new FetchBufferInterface(parameter))

  val fetchBuffer = Reg(Vec(parameter.fetchBufferEntries, new FetchPacket(parameter)))
  val fetchBufferMatrix = Wire(Vec(parameter.fetchPacketNum, Vec(parameter.decodeWidth, new FetchPacket(parameter))))

  val head1H = RegInit(1.U(parameter.fetchPacketNum.W))
  val tail1H = RegInit(1.U(parameter.fetchBufferEntries.W))

  def inc(ptr: UInt): UInt = {
    val n = ptr.getWidth
    Cat(ptr(n-2, 0), ptr(n-1))
  }

  for (i <- 0 until parameter.fetchBufferEntries) {
    fetchBufferMatrix(i/parameter.decodeWidth)(i%parameter.decodeWidth) := fetchBuffer(i)
  }

  val inMask: Vec[Bool] = cutUInt(io.enq.bits.mask, 1).asTypeOf(Vec(parameter.fetchWidth, Bool()))
  val inUops: Vec[FetchPacket] = VecInit(io.enq.bits.inst.zipWithIndex.map {case (inst, index) =>
    val fetchPacket = new FetchPacket(parameter)
    fetchPacket.inst := inst
    fetchPacket.pc := io.enq.bits.pc(parameter.paddrBits-1, log2Ceil(parameter.fetchWidth)) ## index.asUInt(log2Ceil(parameter.fetchWidth).W)
    fetchPacket
  })

  val enqIdx1H = Wire(Vec(parameter.fetchWidth, UInt(parameter.fetchBufferEntries.W)))
  var enqIdx = tail1H
  for (i <- 0 until parameter.fetchWidth) {
    enqIdx1H(i) := enqIdx
    enqIdx = Mux(inMask(i), inc(enqIdx), enqIdx)
  }

  for (i <- 0 until parameter.fetchWidth) {
    for (j <- 0 until parameter.fetchBufferEntries) {
      when (inMask(i) && enqIdx1H(i)(j)) {
        fetchBuffer(j) := inUops(i)
      }
    }
  }

  val mayFull = RegInit(false.B)

  def rotateLeft(in: UInt, k: Int): UInt = {
    val n = in.getWidth
    in(n-k-1, 0) ## in(n-1, n-k)
  }

  val mayHitHead = (1 until parameter.fetchWidth).map { k =>
    VecInit(rotateLeft(tail1H, k).asBools.zipWithIndex.filter { case (bit,idx) =>
      idx % parameter.decodeWidth == 0
    }.map { case (bit, idx) => bit }).asUInt
  }.map { newTail => head1H & newTail }.reduce(_|_).orR

  val atHead = (
    VecInit(tail1H.asBools.zipWithIndex.filter { case (bit, idx) =>
      idx % parameter.decodeWidth == 0
    }.map { case (bit, idx) => bit }).asUInt & head1H
  ).orR

  val doEnqueue = !((atHead && mayFull) || mayHitHead)

  val mayHitTail = VecInit((0 until parameter.fetchBufferEntries).map(idx =>
    head1H(idx / parameter.decodeWidth) && (!mayFull || (idx % parameter.decodeWidth != 0).B)
  )).asUInt & tail1H

  val slotWillHitTail = (0 until parameter.fetchPacketNum).map { i =>
    mayHitTail((i + 1) * parameter.decodeWidth - 1, i * parameter.decodeWidth)
  }.reduce(_|_)

  val willHitTail = slotWillHitTail.orR

  val doDequeue = io.deq.ready && !willHitTail
  val deqValid = (~MaskUpper(slotWillHitTail)).asBools

  io.enq.ready := doEnqueue
  io.deq.valid := deqValid.reduce(_||_)  // why !willHitTail  for little decodewidth output
  io.deq.bits.uops.zip(deqValid).map { case (d, v) => d.valid := v }  // each output is valid?
  io.deq.bits.uops.zip(Mux1H(head1H, fetchBufferMatrix)).map { case (d, q) => d.bits := q }

  when (io.enq.fire) {
    tail1H := enqIdx
    mayFull := true.B
  }

  when (doDequeue) {
    head1H := inc(head1H)
    mayFull := false.B
  }

  when (io.clear) {
    head1H := 1.U
    tail1H := 1.U
    mayFull := false.B
  }
}
