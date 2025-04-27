package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import groom._

object BusyTableParameter {
  implicit def rwP: upickle.default.ReadWriter[BusyTableParameter] = upickle.default.macroRW[BusyTableParameter]
}

case class BusyTableParameter(
  renameWidth: Int,
  wakeUpWidth: Int,
  pregNum: Int,
  delaySize: Int,
) extends SerializableModuleParameter {
  val pregSize = log2Ceil(pregNum)
}

class Preg(parameter: BusyTableParameter) extends Bundle {
  val psrc = Vec(2, UInt(parameter.pregSize.W))
  val pdst = UInt(parameter.pregSize.W)
}

class BusyTableInterface(parameter: BusyTableParameter) extends Bundle {
  val redirect = Input(Bool())
  val preg = Input(Vec(parameter.renameWidth, new Preg(parameter)))
  val busyReq = Input(Vec(parameter.renameWidth, Bool()))
  val psrcBusy = Output(Vec(parameter.renameWidth, Vec(2, Bool())))

  val wakeUpReq = Vec(parameter.wakeUpWidth, Flipped(Valid(new WakeUpReq(parameter.pregNum, parameter.delaySize))))
}

@instantiable
class BusyTable(val parameter: BusyTableParameter) extends Module with SerializableModule[BusyTableParameter] {

  @public
  val io = IO(new BusyTableInterface(parameter))

  val busyTable = RegInit(0.U(parameter.pregNum.W))

  val busyTableWakeup = busyTable &
    ~(io.wakeUpReq).map { pdst =>
      pdst.valid.asUInt << pdst.bits.pdst
    }.reduce(_|_)

  val busyTableNext = busyTableWakeup |
    io.preg.zip(io.busyReq).map { case (preg, req) =>
      req.asUInt << preg.pdst
    }.reduce(_|_)

  busyTable := Mux(io.redirect, 0.U, busyTableNext)

  io.psrcBusy.zip(io.preg).map { case (busy, preg) =>
    busy(0) := busyTable(preg.psrc(0))
    busy(1) := busyTable(preg.psrc(1))
  }
}