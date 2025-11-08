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
  val isLoad = Bool()
}

class DispatchInterface(parameter: DispatchParameter) extends Bundle {
  val renameVld = Flipped(DecoupledIO(Vec(parameter.renameWidth, new RenameVld())))
  val intIqVld = Output(Vec(parameter.dispatchWidth, Bool()))
  val intIqRdy = Input(Vec(parameter.dispatchWidth, Bool()))
  val ldaIqVld = Output(Vec(parameter.dispatchWidth, Bool()))
  val ldaIqRdy = Input(Vec(parameter.dispatchWidth, Bool()))
  val staIqVld = Output(Vec(parameter.dispatchWidth, Bool()))
  val staIqRdy = Input(Vec(parameter.dispatchWidth, Bool()))
  val stdIqVld = Output(Vec(parameter.dispatchWidth, Bool()))
  val stdIqRdy = Input(Vec(parameter.dispatchWidth, Bool()))
  val robVld = Output(Bool())
  val robRdy = Input(Bool())
  val sqVld = Output(Bool())
  val sqRdy = Input(Bool())
  val lqVld = Output(Bool())
  val lqRdy = Input(Bool())
}

@instantiable
class Dispatch(val parameter: DispatchParameter) extends Module with SerializableModule[DispatchParameter] {

  @public
  val io = IO(new DispatchInterface(parameter))

  val intIqVld = io.renameVld.bits.map(!_.instType && io.renameVld.valid)
  val intIqVldNum = PopCount(intIqVld)
  val intIqVldNumOH = Seq.tabulate(parameter.dispatchWidth)(i => (i+1).U===intIqVldNum)
  val intIqMatch = intIqVldNumOH.zip(io.intIqRdy).map { case (vld, rdy) => vld && rdy }.reduce(_||_)

  io.intIqVld := intIqVld.map(_ && io.renameVld.ready)

  val ldaIqVld = io.renameVld.bits.map(v => v.instType && v.isLoad && io.renameVld.valid)
  val ldaIqVldNum = PopCount(ldaIqVld)
  val ldaIqVldNumOH = Seq.tabulate(parameter.dispatchWidth)(i => (i+1).U===ldaIqVldNum)
  val ldaIqMatch = ldaIqVldNumOH.zip(io.ldaIqRdy).map { case (vld, rdy) => vld && rdy }.reduce(_||_)

  val staIqVld = io.renameVld.bits.map(v => v.instType && !v.isLoad && io.renameVld.valid)
  val staIqVldNum = PopCount(staIqVld)
  val staIqVldNumOH = Seq.tabulate(parameter.dispatchWidth)(i => (i+1).U===staIqVldNum)
  val staIqMatch = staIqVldNumOH.zip(io.staIqRdy).map { case (vld, rdy) => vld && rdy }.reduce(_||_)

  val stdIqVld = io.renameVld.bits.map(v => v.instType && !v.isLoad && io.renameVld.valid)
  val stdIqVldNum = PopCount(stdIqVld)
  val stdIqVldNumOH = Seq.tabulate(parameter.dispatchWidth)(i => (i+1).U===stdIqVldNum)
  val stdIqMatch = stdIqVldNumOH.zip(io.stdIqRdy).map { case (vld, rdy) => vld && rdy }.reduce(_||_)

  io.ldaIqVld := ldaIqVld.map(_ && io.renameVld.ready)
  io.staIqVld := staIqVld.map(_ && io.renameVld.ready)
  io.stdIqVld := stdIqVld.map(_ && io.renameVld.ready)

  io.renameVld.ready := intIqMatch && ldaIqMatch && staIqMatch && stdIqMatch
                        io.robRdy && io.sqRdy && io.lqRdy
  io.robVld := io.renameVld.fire
  io.lqVld := io.renameVld.fire
  io.sqVld := io.renameVld.fire
}
