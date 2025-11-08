package groom.rtl.frontend

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.experimental.hierarchy.{instantiable, public, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import org.chipsalliance.amba.axi4.bundle.{
  AXI4BundleParameter,
  AXI4ChiselBundle,
  AXI4ROIrrevocable,
  AXI4RWIrrevocable,
  R,
  W
}

case class DCacheParameter(
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
  val loadStoreParameter: AXI4BundleParameter = AXI4BundleParameter(
    idWidth = 1,
    dataWidth = rowBits,
    addrWidth = paddrBits,
    userReqWidth = 0,
    userDataWidth = 0,
    userRespWidth = 0,
    hasAW = true,
    hasW = true,
    hasB = true,
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

class DCacheInterface(parameter: DCacheParameter) extends Bundle {
  val loadStoreAXI: AXI4ROIrrevocable =
    org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable(parameter.loadStoreParameter)
}

@instantiable
class DCache(val parameter: DCacheParameter) extends Module with SerializableModule[DCacheParameter] {
  @public
  val io = IO(new DCacheInterface(parameter))

  val sNormal :: sFetchAr :: sFetchR :: Nil = Enum(3)
  val dCacheState = RegInit(sNormal)

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
    parameter.nSets * parameter.nWays,
    UInt((parameter.blockBytes*8).W)
  )

}
