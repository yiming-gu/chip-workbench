package groom.rtl.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.t1.rtl._

case class FrontendParameter(
  fetchBytes: Int,
  instBytes: Int,
  nSets:      Int,
  nWays:      Int,
  paddrBits:  Int,
  blockBytes: Int,
  busBytes:   Int,
  decodeWidth: Int,
  fetchBufferEntries: Int
) {
  val fetchWidth = fetchBytes / instBytes

  def iCacheParameter: ICacheParameter = ICacheParameter(
    fetchBytes = fetchBytes,
    nSets      = nSets,
    nWays      = nWays,
    paddrBits  = paddrBits,
    blockBytes = blockBytes,
    busBytes   = busBytes
  )

  def fetchBufferParameter: FetchBufferParameter = FetchBufferParameter(
    fetchWidth = fetchWidth,
    instBytes = instBytes,
    paddrBits = paddrBits,
    decodeWidth = decodeWidth,
    fetchBufferEntries = fetchBufferEntries
  )
}

class FrontendInterface(parameter: FrontendParameter) extends Bundle {

}

class Frontend(parameter: FrontendParameter) extends Bundle {

  val snpc = Wire(UInt(32.W))
  val pc = RegEnable(snpc, "h80000000".U(32.W), iCache.io.req.fire)

  snpc := NextPc(pc, parameter.fetchBytes, parameter.paddrBits)

  val dnpc = MuxCase(pc, Seq(

  ))

  val iCache = Module(new ICache(parameter.iCacheParameter))

  val fetchBuffer = Module(new FetchBuffer(parameter.fetchBufferParameter))

  iCache.io.req.bits.addr := dnpc
  iCache.io.req.valid := true.B
  iCache.io.resp.ready := fetchBuffer.io.enq.ready

  fetchBuffer.io.enq.valid := iCache.io.resp.valid
  fetchBuffer.io.enq.bits.inst := cutUInt(iCache.io.resp.bits.data, parameter.instBytes*8)
  fetchBuffer.io.enq.bits.pc := iCache.io.resp.bits.pc(parameter.paddrBits-1, log2Ceil(parameter.fetchBytes)) ## 0.U(log2Ceil(parameter.fetchBytes).W)
  fetchBuffer.io.enq.bits.mask := MuxLookup(iCache.io.resp.bits.pc(log2Ceil(parameter.fetchBytes)-1, log2Ceil(parameter.instBytes)), 0.U)(Seq(
    0.U -> "b1111".U,
    1.U -> "b1110".U,
    2.U -> "b1100".U,
    3.U -> "b1000".U,
  ))
}