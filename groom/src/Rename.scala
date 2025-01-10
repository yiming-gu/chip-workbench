package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.DecodeBundle
import org.chipsalliance.rocketv.DecoderParameter
import org.chipsalliance.amba.axi4.bundle.chisel

case class RenameParameter(
  renameWidth: Int,
  robCommitWidth: Int,
  pregNum: Int,
  lregNum: Int,
  instBytes: Int,
  paddrBits: Int,
) {
  val pregSize = log2Ceil(pregNum)
  val lregSize = log2Ceil(lregNum)

  def renameTableParameter: RenameTableParameter = RenameTableParameter(
    renameWidth = renameWidth,
    robCommitWidth = robCommitWidth,
    pregNum = pregNum,
    lregNum = lregNum,
  )

  def freeListParameter: FreeListParameter = FreeListParameter(
    renameWidth = renameWidth,
    pregNum = pregNum,
  )
}

class DecodeLreg(parameter: RenameParameter) extends Bundle {
  val lsrcValid = Vec(2, Bool())
  val lsrc = Vec(2, UInt(parameter.lregSize.W))
  val ldstValid = Bool()
  val ldst = UInt(parameter.lregSize.W)
}

class RenamePreg(parameter: RenameParameter) extends Bundle {
  val psrc = Vec(2, UInt(parameter.pregSize.W))
  val pdst = UInt(parameter.pregSize.W)
}

class RenameReq(parameter: RenameParameter) extends Bundle {
  val pc   = UInt(parameter.paddrBits.W)
  val inst = Vec(parameter.renameWidth, UInt((parameter.instBytes*8).W))
  val decodeLreg = Vec(parameter.renameWidth, new DecodeLreg(parameter))
}

class RenameResp(parameter: RenameParameter) extends Bundle {
  val pc   = UInt(parameter.paddrBits.W)
  val inst = Vec(parameter.renameWidth, UInt((parameter.instBytes*8).W))
  val renamePreg = Vec(parameter.renameWidth, new RenamePreg(parameter))
}

class RenameInterface(parameter: RenameParameter) extends Bundle {
  val renameReq = Flipped(Decoupled(new RenameReq(parameter)))
  val renameResp = Decoupled(new RenameResp(parameter))
}

class Rename(parameter: RenameParameter) extends Module {
  val io = IO(new RenameInterface(parameter))

  val preg = Vec(parameter.renameWidth, new RenamePreg(parameter))
  val renameTable = Module(new RenameTable(parameter.renameTableParameter))
  val freeList = Module(new FreeList(parameter.freeListParameter))

  (renameTable.io.specTableRead zip io.renameReq.bits.decodeLreg zip preg) map { case ((table, lreg), preg) =>
    table.addr(0) := lreg.lsrc(0)
    table.addr(1) := lreg.lsrc(1)
    preg.psrc(0) := table.data(0)
    preg.psrc(1) := table.data(1)
  }

  freeList.io.allocateReq := io.renameReq.bits.decodeLreg.map(_.ldstValid)
  (preg zip freeList.io.allocatePhyReg) map { case (preg, apreg) =>
    preg.pdst := apreg
  }

  (renameTable.io.specTableWrite zip io.renameReq.bits.decodeLreg zip preg).zipWithIndex.map { case (((table, lreg), preg), i) =>
    if (i == parameter.renameWidth-1) {
      table.wen := lreg.ldstValid
    }
    else {
      table.wen := !(bypassCond(2).takeRight(parameter.renameWidth-i-1).reduce(_(i)&&_(i)).asBool) && lreg.ldstValid
    }
    table.addr := lreg.ldst
    table.data := preg.pdst
  }

  val bypassCond: Vec[MixedVec[UInt]] = Wire(Vec(2 + 1, MixedVec(List.tabulate(parameter.renameWidth-1)(i => UInt((i+1).W)))))
  for (i <- 1 until parameter.renameWidth) {
    val intCond = io.renameReq.bits.decodeLreg(i).lsrcValid :+ io.renameReq.bits.decodeLreg(i).ldstValid
    val target = io.renameReq.bits.decodeLreg(i).lsrc :+ io.renameReq.bits.decodeLreg(i).ldst
    for (((cond, t), j) <- intCond.zip(target).zipWithIndex) {
      val destToSrc = io.renameReq.bits.decodeLreg.take(i).zipWithIndex.map { case (lreg, j) =>
        val indexMatch = lreg.ldst === t
        val writeMatch = cond && lreg.ldstValid
        indexMatch && writeMatch
      }
      bypassCond(j)(i - 1) := VecInit(destToSrc).asUInt
    }
    io.renameResp.bits.renamePreg(i).psrc(0) := io.renameResp.bits.renamePreg.take(i).map(_.pdst).zip(bypassCond(0)(i-1).asBools).foldLeft(preg(i).psrc(0)) {
      (z, next) => Mux(next._2, next._1, z)
    }
    io.renameResp.bits.renamePreg(i).psrc(1) := io.renameResp.bits.renamePreg.take(i).map(_.pdst).zip(bypassCond(0)(i-1).asBools).foldLeft(preg(i).psrc(1)) {
      (z, next) => Mux(next._2, next._1, z)
    }
  }

}
