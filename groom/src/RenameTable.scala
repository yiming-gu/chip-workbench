package groom.rtl.backend

import chisel3._
import chisel3.util._

case class RenameTableParameter(
  renameWidth: Int,
  robCommitWidth: Int,
  pregNum: Int,
  lregNum: Int,
) {
  val pregSize = log2Ceil(pregNum)
  val lregSize = log2Ceil(lregNum)
}

class SpecTableRead(parameter: RenameTableParameter) extends Bundle {
  val addr = Input(Vec(2, UInt(parameter.lregSize.W)))
  val data = Output(Vec(2, UInt(parameter.pregSize.W)))
}

class SpecTableWrite(parameter: RenameTableParameter) extends Bundle {
  val wen = Bool()
  val addr = UInt(parameter.lregSize.W)
  val data = UInt(parameter.pregSize.W)
}

class ArchTableWrite(parameter: RenameTableParameter) extends Bundle {
  val wen = Bool()
  val addr = UInt(parameter.lregSize.W)
  val data = UInt(parameter.pregSize.W)
}

class RenameTableInterface(parameter: RenameTableParameter) extends Bundle {
  val specTableRead = Input(Vec(parameter.renameWidth, new SpecTableRead(parameter)))
  val specTableWrite = Output(Vec(parameter.renameWidth, new SpecTableWrite(parameter)))
  val archTableWrite = Output(Vec(parameter.robCommitWidth, new ArchTableWrite(parameter)))
}

class RenameTable(parameter: RenameTableParameter) extends Module {
  val io = IO(new RenameTableInterface(parameter))

  val specTable = RegInit(VecInit(Seq.fill(parameter.lregNum) { 0.U(parameter.lregNum.W) }))
  val archTable = RegInit(VecInit(Seq.fill(parameter.lregNum) { 0.U(parameter.lregNum.W) }))

  io.specTableRead.foreach { r =>
    r.data(0) := specTable(r.addr(0))
    r.data(1) := specTable(r.addr(1))
  }
  io.specTableWrite.foreach { w =>
    when(w.wen) {
      specTable(w.addr) := w.data
    }
  }
  io.archTableWrite.foreach { w =>
    when(w.wen) {
      archTable(w.addr) := w.data
    }
  }
}
