package groom.rtl.frontend

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate, public}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import org.chipsalliance.amba.axi4.bundle.{AXI4BundleParameter, AXI4ROIrrevocable, AXI4RWIrrevocable}
import org.chipsalliance.t1.rtl._
import groom._

object FrontendParameter {
  implicit def rwP: upickle.default.ReadWriter[FrontendParameter] = upickle.default.macroRW[FrontendParameter]
}

case class FrontendParameter(
  xLen:       Int,
  fetchBytes: Int,
  instBytes:  Int,
  nSets:      Int,
  nWays:      Int,
  paddrBits:  Int,
  blockBytes: Int,
  busBytes:   Int,
  decodeWidth: Int,
  fetchBufferEntries: Int
) extends SerializableModuleParameter {
  val fetchWidth = fetchBytes / instBytes

  val iCacheParameter: ICacheParameter = ICacheParameter(
    fetchBytes = fetchBytes,
    nSets      = nSets,
    nWays      = nWays,
    paddrBits  = paddrBits,
    blockBytes = blockBytes,
    busBytes   = busBytes
  )

  val fetchBufferParameter: FetchBufferParameter = FetchBufferParameter(
    fetchWidth = fetchWidth,
    paddrBits = paddrBits,
    decodeWidth = decodeWidth,
    fetchBufferEntries = fetchBufferEntries
  )
}

class FrontendInterface(parameter: FrontendParameter) extends Bundle {
  // val pc = Input(UInt(32.W))
  val redirect = Flipped(ValidIO(new Redirect(parameter.xLen)))
  val frontendPacket = DecoupledIO(new FetchBufferResp(parameter.fetchBufferParameter))
  val instructionFetchAXI: AXI4ROIrrevocable =
    org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable(parameter.iCacheParameter.instructionFetchParameter)
  val iCacheFire = Output(Bool())
}

@instantiable
class Frontend(val parameter: FrontendParameter) extends Module with SerializableModule[FrontendParameter] {
  @public
  val io = IO(new FrontendInterface(parameter))

  val iCache: Instance[ICache] = Instantiate(new ICache(parameter.iCacheParameter))
  val fetchBuffer: Instance[FetchBuffer] = Instantiate(new FetchBuffer(parameter.fetchBufferParameter))

  val snpc = Wire(UInt(32.W))
  val dnpc = Wire(UInt(32.W))
  val pc = RegEnable(Mux(io.redirect.valid, dnpc, snpc), "h80000000".U(32.W), iCache.io.req.fire || io.redirect.valid)
  // val pc = io.pc

  dnpc := MuxCase(pc, Seq(
    io.redirect.valid -> io.redirect.bits.pc,
    iCache.io.cacheMissJump -> iCache.io.cacheMissJumpPc,
  ))

  snpc := NextPc(dnpc, parameter.fetchBytes, parameter.paddrBits)

  iCache.io.req.bits.addr := dnpc
  iCache.io.req.valid := true.B
  iCache.io.resp.ready := fetchBuffer.io.enq.ready

  iCache.io.s1Kill := io.redirect.valid
  iCache.io.s2Kill := io.redirect.valid

  fetchBuffer.io.enq.valid := iCache.io.resp.valid
  fetchBuffer.io.enq.bits.inst := cutUInt(iCache.io.resp.bits.data, parameter.instBytes*8)
  fetchBuffer.io.enq.bits.pc := iCache.io.resp.bits.pc(parameter.paddrBits-1, log2Ceil(parameter.fetchBytes)) ## 0.U(log2Ceil(parameter.fetchBytes).W)
  fetchBuffer.io.enq.bits.mask := VecInit(Seq.fill(parameter.fetchWidth)(true.B)).asUInt << iCache.io.resp.bits.pc(log2Ceil(parameter.fetchBytes)-1, log2Ceil(parameter.instBytes))
  fetchBuffer.io.clear := io.redirect.valid

  io.frontendPacket <> fetchBuffer.io.deq
  io.instructionFetchAXI <> iCache.io.instructionFetchAXI
  io.iCacheFire := iCache.io.req.fire
}