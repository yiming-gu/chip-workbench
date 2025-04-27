package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import org.chipsalliance.t1.rtl._

object DispatchParameter {
  implicit def rwP: upickle.default.ReadWriter[DispatchParameter] = upickle.default.macroRW[DispatchParameter]
}

case class DispatchParameter(
  renameWidth: Int,
  dispatchWidth: Int,
  issueQueueNum: Int,
) extends SerializableModuleParameter {

}

class RenameVld extends Bundle {
  val instType = Bool()
}

class DispatchInterface(parameter: DispatchParameter) extends Bundle {
  val renameVld = Flipped(DecoupledIO(Vec(parameter.renameWidth, new RenameVld())))
  val intIqVld = Output(Vec(parameter.dispatchWidth, Bool()))
  val intIqRdy = Input(Vec(parameter.dispatchWidth, Bool()))
  // val memIqVld = Output(Vec(parameter.dispatchWidth, Bool()))
  // val memIqRdy = Input(Vec(parameter.dispatchWidth, Bool()))
  val robVld = Output(Bool())
  val robRdy = Input(Bool())
}

@instantiable
class Dispatch(val parameter: DispatchParameter) extends Module with SerializableModule[DispatchParameter] {

  @public
  val io = IO(new DispatchInterface(parameter))

  val intIqVld = io.renameVld.bits.map(!_.instType && io.renameVld.valid)
  val intIqVldNum = PopCount(intIqVld)
  val intIqVldNumOH = Seq.tabulate(parameter.dispatchWidth)(i => (i+1).U===intIqVldNum)
  val intIqMatch = intIqVldNumOH.zip(io.intIqRdy).map { case (vld, rdy) => vld && rdy }.reduce(_||_)

  io.intIqVld := intIqVld.map(_ && intIqMatch && io.robRdy)

  // io.renameVld.bits.zipWithIndex.foreach { case (rvld, i) =>
  //   io.intIqVld(i) := intIqVld
  //   // io.memIqVld(i) := rvld.instType && io.renameVld.valid
  // }

  // val memIqFire = VecInit(io.memIqVld.zip(io.memIqRdy).map { case (vld, rdy) => vld && rdy })

  // io.renameVld.ready := (intIqFire.asUInt | memIqFire.asUInt).andR
  io.renameVld.ready := intIqMatch && io.robRdy
  io.robVld := io.renameVld.fire
}
