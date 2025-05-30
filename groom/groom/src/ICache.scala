package groom.rtl.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import org.chipsalliance.amba.axi4.bundle.{AXI4BundleParameter, AXI4ROIrrevocable, AXI4RWIrrevocable}
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}

case class ICacheParameter(
  fetchBytes: Int,
  paddrBits:  Int,
  nSets:      Int,
  nWays:      Int,
  blockBytes: Int,
  busBytes:   Int
) extends SerializableModuleParameter {
  val rowBits:                   Int                 = busBytes * 8
  val blockOffBits:              Int                 = log2Ceil(blockBytes)
  val idxBits:                   Int                 = log2Ceil(nSets)
  val untagBits:                 Int                 = blockOffBits + idxBits
  val tagBits:                   Int                 = paddrBits - untagBits
  val instructionFetchParameter: AXI4BundleParameter = AXI4BundleParameter(
    idWidth = 1,
    dataWidth = rowBits,
    addrWidth = paddrBits,
    userReqWidth = 0,
    userDataWidth = 0,
    userRespWidth = 0,
    hasAW = false,
    hasW = false,
    hasB = false,
    hasAR = true,
    hasR = true,
    supportId = true,
    supportRegion = false,
    supportLen = true,
    supportSize = true,
    supportBurst = true,
    supportLock = false,
    supportCache = false,
    supportQos = false,
    supportStrb = false,
    supportResp = false,
    supportProt = false
  )
}

class ICacheReq(parameter: ICacheParameter) extends Bundle {
  val addr = UInt(parameter.paddrBits.W)
}

class ICacheResp(parameter: ICacheParameter) extends Bundle {
  val pc   = UInt(parameter.paddrBits.W)
  val data = UInt((parameter.fetchBytes*8).W)
}

class ICacheInterface(parameter: ICacheParameter) extends Bundle {
  val req  = Flipped(Decoupled(new ICacheReq(parameter)))
  val resp = Decoupled(new ICacheResp(parameter))

  val s1Kill = Input(Bool())
  val s2Kill = Input(Bool())

  val cacheMissJump = Output(Bool())
  val cacheMissJumpPc = Output(UInt(parameter.paddrBits.W))

  val instructionFetchAXI: AXI4ROIrrevocable =
    org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable(parameter.instructionFetchParameter)
}

@instantiable
class ICache(val parameter: ICacheParameter) extends Module with SerializableModule[ICacheParameter] {
  @public
  val io = IO(new ICacheInterface(parameter))

  val sNormal :: sFetchAr :: sFetchR :: Nil = Enum(3)
  val iCacheState = RegInit(sNormal)

  val s0Idx = io.req.bits.addr(parameter.untagBits-1, parameter.blockOffBits)

  val s1s2Ready: Bool = Wire(Bool())
  val s1Addr: UInt = RegEnable(io.req.bits.addr, 0.U, io.req.fire)
  val s1ValidNoKill: Bool = RegEnable(Mux(io.s1Kill || (!io.req.ready && s1s2Ready), false.B, io.req.valid), false.B, io.req.ready | io.s1Kill | (!io.req.ready && s1s2Ready))
  val s1Valid: Bool = !io.s1Kill && s1ValidNoKill
  val s1Tag  = s1Addr(parameter.paddrBits-1, parameter.untagBits)
  val s1Idx  = s1Addr(parameter.untagBits-1, parameter.blockOffBits)
  val s1FetchIdx = s1Addr(parameter.blockOffBits-1, log2Ceil(parameter.fetchBytes))

  val validArrayEn = Wire(Bool())
  val validArrayNext: UInt = Wire(UInt((parameter.nSets * parameter.nWays).W))
  val validArray = RegEnable(
    validArrayNext,
    0.U((parameter.nSets * parameter.nWays).W),
    validArrayEn
  )
  val tagArray   = Mem(
    parameter.nSets,
    Vec(parameter.nWays, UInt(parameter.tagBits.W))
  )
  val dataArray  = Mem(
    parameter.nSets * parameter.nWays * (parameter.blockBytes/parameter.fetchBytes),
    UInt((parameter.fetchBytes*8).W)
  )
  val tagReadData: Vec[UInt] = RegEnable(tagArray.read(s0Idx), VecInit.fill(parameter.nWays)(0.U(parameter.tagBits.W)), io.req.fire)

  // val tagReadData: Vec[UInt] = tagArray.read(s0Idx, io.req.fire)
  val s1TagHit: UInt = VecInit(tagReadData.zipWithIndex.map { case (tag, index) =>
    val s1ValidBit = validArray(s1Idx ## index.U(log2Ceil(parameter.nWays).W))
    s1ValidBit && tag === s1Tag
  }).asUInt

  val s1Hit: Bool = s1TagHit.orR
  val s1HitWayNum: UInt = OHToUInt(s1TagHit)
  val s1s2Valid: Bool = s1Valid
  val s1s2Fire: Bool = s1s2Valid && s1s2Ready

  val s2ValidNoKill: Bool = RegEnable(Mux(io.s2Kill, false.B, s1Valid), false.B, s1s2Ready | io.s2Kill)
  val s2Valid: Bool = !io.s2Kill && s2ValidNoKill
  val s2Hit: Bool = RegEnable(s1Hit, false.B, s1s2Fire)
  val s2HitWayNum: UInt = RegEnable(s1HitWayNum, 0.U, s1s2Fire)
  val s2Addr: UInt = RegEnable(s1Addr, 0.U, s1s2Fire)

  val data: UInt = RegEnable(dataArray.read(s1Idx ## s1HitWayNum ## s1FetchIdx), 0.U, s1s2Fire)
  // val data: UInt = dataArray.read(s1Idx ## s1HitWayNum ## s1FetchIdx, s1s2Fire)

  s1s2Ready := !s2Valid || io.resp.ready

  val refillIdx = s2Addr(parameter.untagBits-1, parameter.blockOffBits)
  val refillTag = s2Addr(parameter.paddrBits-1, parameter.untagBits)
  val replWay = LFSR(log2Ceil(parameter.nWays), s1Valid && ~s2Hit && iCacheState === sNormal)
  val refillBufWriteEn = io.instructionFetchAXI.r.fire
  val refillData = Wire(UInt((parameter.fetchBytes*8).W))
  val refillWriteCntNext = Wire(UInt(log2Ceil(parameter.fetchBytes/parameter.busBytes).W))
  val refillCntNext = Wire(UInt(log2Ceil(parameter.blockBytes/parameter.fetchBytes).W))
  val refillBuf = RegEnable(refillData, 0.U, refillBufWriteEn)
  val refillWriteCnt = RegEnable(refillWriteCntNext, 0.U, RegNext(refillBufWriteEn && !io.instructionFetchAXI.r.bits.last))
  val refillWriteEn = refillWriteCnt === 3.U
  val refillCnt = RegEnable(refillCntNext, 0.U, refillWriteEn)
  val refillLast = io.instructionFetchAXI.r.bits.last
  refillData := io.instructionFetchAXI.r.bits.data ## refillBuf(parameter.fetchBytes*8-1, parameter.busBytes*8)
  refillWriteCntNext := refillWriteCnt + 1.U
  refillCntNext := refillCnt + 1.U

  when (refillWriteEn) {
    dataArray.write(refillIdx ## replWay ## refillCnt, refillBuf)
  }
  when (refillLast) {
    tagArray.write(
      refillIdx,
      VecInit(Seq.fill(parameter.nWays)(refillTag)),
      Seq.tabulate(parameter.nWays)(replWay === _.U)
    )
  }
  validArrayEn := refillLast
  validArrayNext := validArray.bitSet(refillIdx ## replWay, true.B)

  when (iCacheState === sNormal) {
    iCacheState := Mux(s1s2Fire && !s1Hit, sFetchAr, sNormal)
  }
  .elsewhen (iCacheState === sFetchAr) {
    iCacheState := Mux(io.instructionFetchAXI.ar.fire, sFetchR, sFetchAr)
  }
  .elsewhen (iCacheState === sFetchR) {
    iCacheState := Mux(refillLast, sNormal, sFetchR)
  }
  .otherwise {
    iCacheState := iCacheState
  }

  io.instructionFetchAXI.ar.valid := iCacheState === sFetchAr
  io.instructionFetchAXI.ar.bits := DontCare
  io.instructionFetchAXI.ar.bits.id    := 0.U
  io.instructionFetchAXI.ar.bits.addr  := s2Addr(parameter.paddrBits-1, parameter.blockOffBits) ## 0.U(parameter.blockOffBits.W)
  io.instructionFetchAXI.ar.bits.size  := log2Ceil(parameter.busBytes).U
  io.instructionFetchAXI.ar.bits.len   := ((parameter.blockBytes/parameter.busBytes) - 1).U
  io.instructionFetchAXI.ar.bits.burst := 1.U
  io.instructionFetchAXI.r.ready := iCacheState === sFetchR

  io.req.ready := (iCacheState === sNormal && (!s1Valid || s1Hit)) && s1s2Ready
  io.resp.valid := s2Valid && s2Hit
  io.resp.bits.data := data
  io.resp.bits.pc := s2Addr
  io.cacheMissJump := RegNext(refillLast)
  io.cacheMissJumpPc := s2Addr
}
