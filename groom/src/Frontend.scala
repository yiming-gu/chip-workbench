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
  iCache.io.req.bits.addr := dnpc
  iCache.io.req.valid := true.B

  val fetchBuffer = Module(new FetchBuffer(parameter.fetchBufferParameter))
  fetchBuffer.io.enq.bits.inst := cutUInt(iCache.io.resp.bits.data, parameter.instBytes*8)
  fetchBuffer.io.enq.bits.pc := iCache.io.resp.bits.pc
}