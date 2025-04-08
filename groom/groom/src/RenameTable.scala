package groom.rtl.backend

import chisel3._
import chisel3.util._

case class RenameTableParameter(
  renameWidth: Int,
  commitWidth: Int,
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
  val redirect = Input(Bool())
  val specTableRead = Vec(parameter.renameWidth, new SpecTableRead(parameter))
  val specTableWrite = Input(Vec(parameter.renameWidth, new SpecTableWrite(parameter)))
  val archTableWrite = Input(Vec(parameter.commitWidth, new ArchTableWrite(parameter)))
  val oldPdest = Output(Vec(parameter.commitWidth, UInt(parameter.pregSize.W)))
  // val needFree = Output(Vec(parameter.commitWidth, Bool()))
}

class RenameTable(parameter: RenameTableParameter) extends Module {
  val io = IO(new RenameTableInterface(parameter))

  val specTable = RegInit(VecInit.tabulate(parameter.lregNum)(_.U(parameter.pregSize.W)))
  val archTable = RegInit(VecInit.tabulate(parameter.lregNum)(_.U(parameter.pregSize.W)))

  val specTableNext = WireInit(specTable)
  val archTableNext = WireInit(archTable)

  val oldPdest = WireInit(VecInit(Seq.fill(parameter.commitWidth)(0.U(parameter.pregSize.W))))
  // val needFree = RegInit(VecInit(Seq.fill(parameter.commitWidth)(false.B)))

  specTable := specTableNext
  archTable := archTableNext

  when (io.redirect) {
    specTableNext := archTable
  }
  .otherwise {
    io.specTableWrite.foreach { w =>
      when (w.wen) {
        specTableNext(w.addr) := w.data
      }
    }
  }

  io.archTableWrite.zipWithIndex.foreach { case (w, i) =>
    when (w.wen) {
      archTableNext(w.addr) := w.data
    }
    val archMask = VecInit(Seq.fill(parameter.pregSize)(w.wen)).asUInt
    oldPdest(i) :=
      MuxCase(archTable(w.addr) & archMask,
              io.archTableWrite.take(i).reverse.map(x => (x.wen && x.addr === w.addr, x.data & archMask)))
  }

  // oldPdest.zip(needFree).zipWithIndex.foreach { case ((old, free), i) =>
  //   val hasDuplicate = oldPdest.take(i).map(_ === old)
  //   val blockedByDup = if (i == 0) false.B else VecInit(hasDuplicate).asUInt.orR
  //   free := VecInit(archTable.map(_ =/= old)).asUInt.andR && !blockedByDup
  // }

  io.oldPdest := oldPdest
  // io.needFree := needFree

  io.specTableRead.foreach { r =>
    r.data(0) := specTable(r.addr(0))
    r.data(1) := specTable(r.addr(1))
  }

  // val specPOH = io.specTableWrite.map(w => Mux(w.wen, UIntToOH(w.addr), 0.U))
  // specTableNext.zipWithIndex.map { case (next, i) =>
  //   val poh = specPOH.map(w => w(i))
  //   val wdata = Mux1H(poh, io.specTableWrite.map(_.data))
  //   next := Mux(io.redirect,
  //     archTable(i),
  //     Mux(poh.reduce(_ || _), wdata, specTable(i))
  //   )
  // }
}
