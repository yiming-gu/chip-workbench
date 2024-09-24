package groom.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.amba.axi4.bundle.{AXI4BundleParameter, AXI4ROIrrevocable, AXI4RWIrrevocable}

case class ICacheParameter(
  fetchBytes: Int,
  nSets:      Int,
  nWays:      Int,
  paddrBits:  Int,
  blockBytes: Int,
  busBytes:   Int
) {
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

class ICacheReq(vaddrBits: Int) extends Bundle {
  val addr = UInt(vaddrBits.W)
}

class ICacheResp(paddrBits: Int, fetchBytes: Int) extends Bundle {
  val pc   = UInt(paddrBits.W)
  val data = UInt((fetchBytes*8).W)
}

class ICacheInterface(parameter: ICacheParameter) extends Bundle {
  val req  = Flipped(Decoupled(new ICacheReq(parameter.paddrBits)))
  val resp = Valid(new ICacheResp(parameter.paddrBits, parameter.fetchBytes))

  val s1Kill = Input(Bool())
  val s2Kill = Input(Bool())

  val instructionFetchAXI: AXI4ROIrrevocable =
    org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable(parameter.instructionFetchParameter)
}

class ICache(parameter: ICacheParameter) extends Module {
  val io = IO(new ICacheInterface(parameter))

  val sNormal :: sFetch :: Nil = Enum(2)
  val iCacheState = RegInit(sNormal)

  val s0Tag = io.req.bits.addr(parameter.paddrBits-1, parameter.untagBits)

  val s1Addr = RegNext(io.req.bits.addr)
  val s1Valid = RegNext(io.req.fire)
  val s1Tag  = s1Addr(parameter.paddrBits-1, parameter.untagBits)
  val s1Idx  = s1Addr(parameter.untagBits-1, parameter.blockOffBits)

  val validArrayEn = Wire(Bool())
  val validArrayNext: UInt = Wire(UInt((parameter.nSets * parameter.nWays).W))
  val validArray = RegEnable(
    validArrayNext,
    0.U((parameter.nSets * parameter.nWays).W),
    validArrayEn)
  val tagArray   = SyncReadMem(
    parameter.nSets,
    Vec(parameter.nWays, UInt(parameter.tagBits.W))
  )

  val tagReadData: Vec[UInt] = tagArray.read(s0Tag)
  val s1TagHit: UInt = VecInit(tagReadData.zipWithIndex.map { case (tag, index) =>
    val s1ValidBit = validArray(s1Idx ## index.U(log2Ceil(parameter.nWays).W))
    s1ValidBit && tag === s1Tag
  }).asUInt

  val s1Hit: Bool = s1TagHit.orR
  val s1HitWayNum: UInt = OHToUInt(s1TagHit)

  val s2Valid: Bool = RegNext(s1Valid && !io.s1Kill)
  val s2Hit: Bool = RegNext(s1Hit)
  val s2HitWayNum: UInt = RegNext(s1HitWayNum)

  io.req.ready := iCacheState === sNormal
  io.resp.valid := s2Valid && s2Hit
  io.resp.bits.data := 0.U
}
