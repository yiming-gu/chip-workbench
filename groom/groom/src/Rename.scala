package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.DecodeBundle
import org.chipsalliance.rocketv.DecoderParameter
import org.chipsalliance.amba.axi4.bundle.chisel
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate, public}
import groom._

case class RenameParameter(
  renameWidth: Int,
  commitWidth: Int,
  pregNum: Int,
  lregNum: Int,
  instBytes: Int,
  paddrBits: Int,
  wakeUpWidth: Int,
  delaySize: Int,
) extends SerializableModuleParameter {
  val pregSize = log2Ceil(pregNum)
  val lregSize = log2Ceil(lregNum)

  def renameTableParameter: RenameTableParameter = RenameTableParameter(
    renameWidth = renameWidth,
    commitWidth = commitWidth,
    pregNum = pregNum,
    lregNum = lregNum,
  )

  def freeListParameter: FreeListParameter = FreeListParameter(
    renameWidth = renameWidth,
    commitWidth = commitWidth,
    lregNum = lregNum,
    pregNum = pregNum,
    freelistNum = pregNum - lregNum,
  )

  def busyTableParameter: BusyTableParameter = BusyTableParameter(
    renameWidth = renameWidth,
    wakeUpWidth = wakeUpWidth,
    pregNum = pregNum,
    delaySize = delaySize,
  )
}

class DecodeLreg(parameter: RenameParameter) extends Bundle {
  val lsrcValid = Vec(2, Bool())
  val lsrc = Vec(2, UInt(parameter.lregSize.W))
  val ldstValid = Bool()
  val ldst = UInt(parameter.lregSize.W)
}

class RenameInterface(parameter: RenameParameter) extends Bundle {
  val redirect = Input(Bool())
  val decodeLreg = Flipped(DecoupledIO(Vec(parameter.renameWidth, new DecodeLreg(parameter))))
  val renamePreg = DecoupledIO(Vec(parameter.renameWidth, new RenamePreg(parameter.pregSize)))

  val wakeUpReq = Vec(parameter.wakeUpWidth, Flipped(ValidIO(new WakeUpReq(parameter.pregNum, parameter.delaySize))))
  val commit = Vec(parameter.commitWidth, Flipped(ValidIO(new RobCommit(
    parameter.lregSize,
    parameter.pregSize
  ))))
}

@instantiable
class Rename(val parameter: RenameParameter) extends Module with SerializableModule[RenameParameter] {

  @public
  val io = IO(new RenameInterface(parameter))

  val renameVld = RegEnable(io.decodeLreg.valid, false.B, io.decodeLreg.ready)
  io.decodeLreg.ready := !renameVld || io.renamePreg.ready
  io.renamePreg.valid := renameVld

  val preg = Wire(Vec(parameter.renameWidth, new RenamePreg(parameter.pregSize)))
  val renameTable = Module(new RenameTable(parameter.renameTableParameter))

  val freeList: Instance[FreeList] = Instantiate(new FreeList(parameter.freeListParameter))
  val busyTable: Instance[BusyTable] = Instantiate(new BusyTable(parameter.busyTableParameter))

  renameTable.io.redirect := io.redirect
  renameTable.io.specTableRead.zip(io.decodeLreg.bits).zip(preg).map { case ((table, lreg), preg) =>
    table.addr(0) := lreg.lsrc(0)
    table.addr(1) := lreg.lsrc(1)
    preg.psrc(0) := table.data(0)
    preg.psrc(1) := table.data(1)
  }

  busyTable.io.preg.zip(preg).map { case (bpreg, preg) =>
    bpreg.psrc(0) := preg.psrc(0)
    bpreg.psrc(1) := preg.psrc(1)
    bpreg.pdst := preg.pdst
  }
  busyTable.io.busyReq := io.decodeLreg.bits.map(_.ldstValid)
  busyTable.io.wakeUpReq := io.wakeUpReq

  preg.zip(busyTable.io.psrcBusy).zip(io.decodeLreg.bits).map { case ((preg, busy), lvld) =>
    preg.psrcBusy(0) := busy(0) && lvld.lsrcValid(0)
    preg.psrcBusy(1) := busy(1) && lvld.lsrcValid(1)
  }

  freeList.io.allocateReq := io.decodeLreg.bits.map(_.ldstValid)
  freeList.io.freeReq := io.commit.map(c => c.valid && c.bits.wxd)
  freeList.io.freePhyReg := renameTable.io.oldPdest
  freeList.io.redirect := false.B
  preg.zip(freeList.io.allocatePhyReg).map { case (preg, apreg) => preg.pdst := apreg }

  val bypassCond: Vec[MixedVec[UInt]] = Wire(Vec(2 + 1, MixedVec(List.tabulate(parameter.renameWidth-1)(i => UInt((i+1).W)))))
  renameTable.io.specTableWrite.zip(io.decodeLreg.bits).zip(preg).zipWithIndex.map { case (((table, lreg), preg), i) =>
    if (i == parameter.renameWidth-1) {
      table.wen := lreg.ldstValid
    }
    else {
      table.wen := !(bypassCond(2).takeRight(parameter.renameWidth-1-i).reduce(_(i)||_(i))) && lreg.ldstValid
    }
    table.addr := lreg.ldst
    table.data := preg.pdst
  }

  renameTable.io.archTableWrite.zip(io.commit).map { case (table, c) =>
    table.wen := c.valid && c.bits.wxd
    table.addr := c.bits.ldst
    table.data := c.bits.pdst
  }

  for (i <- 1 until parameter.renameWidth) {
    val intCond = io.decodeLreg.bits(i).lsrcValid :+ io.decodeLreg.bits(i).ldstValid
    val target = io.decodeLreg.bits(i).lsrc :+ io.decodeLreg.bits(i).ldst
    for (((cond, t), j) <- intCond.zip(target).zipWithIndex) {
      val destToSrc = io.decodeLreg.bits.take(i).zipWithIndex.map { case (lreg, j) =>
        val indexMatch = lreg.ldst === t
        val writeMatch = cond && lreg.ldstValid
        indexMatch && writeMatch
      }
      bypassCond(j)(i - 1) := VecInit(destToSrc).asUInt
    }
    io.renamePreg.bits(i).psrc(0) := io.renamePreg.bits.take(i).map(_.pdst).zip(bypassCond(0)(i-1).asBools).foldLeft(preg(i).psrc(0)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.renamePreg.bits(i).psrc(1) := io.renamePreg.bits.take(i).map(_.pdst).zip(bypassCond(1)(i-1).asBools).foldLeft(preg(i).psrc(1)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.renamePreg.bits(i).psrcBusy(0) := preg(i).psrcBusy(0) || bypassCond(0)(i-1).orR
    io.renamePreg.bits(i).psrcBusy(1) := preg(i).psrcBusy(1) || bypassCond(1)(i-1).orR
  }

  io.renamePreg.bits(0).psrc(0) := preg(0).psrc(0)
  io.renamePreg.bits(0).psrc(1) := preg(0).psrc(1)
  io.renamePreg.bits(0).psrcBusy(0) := preg(0).psrcBusy(0)
  io.renamePreg.bits(0).psrcBusy(1) := preg(0).psrcBusy(1)
  io.renamePreg.bits.zip(preg).map { case (r, p) =>
    r.pdst := p.pdst
  }
}
