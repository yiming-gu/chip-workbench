package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import org.chipsalliance.rocketv._
import org.chipsalliance.t1.rtl._
import groom._

object IssueParameter {
  implicit def rwP: upickle.default.ReadWriter[IssueParameter] = upickle.default.macroRW[IssueParameter]
}

case class IssueParameter(
  paddrBits: Int,
  dispatchWidth: Int,
  issueSlotNum: Int,
  wakeUpWidth: Int,
  issueWidth: Int,
  pregNum: Int,
  delayWidth: Int,
  robNum: Int,
  decoderParameter: DecoderParameter,
) extends SerializableModuleParameter {
  val pregSize = log2Ceil(pregNum)
  val robSize = log2Ceil(robNum)
}

class IssueUnitInterface(parameter: IssueParameter) extends Bundle {
  val disUops = Vec(parameter.dispatchWidth, Flipped(DecoupledIO(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
  val wakeUpReq = Vec(parameter.wakeUpWidth, Flipped(ValidIO(new WakeUpReq(parameter.pregNum, parameter.delayWidth))))
  val issuePacket = Vec(parameter.issueWidth, ValidIO(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  )))
}

@instantiable
class IssueUnit(val parameter: IssueParameter) extends Module with SerializableModule[IssueParameter] {

  @public
  val io = IO(new IssueUnitInterface(parameter))

  val slots = Seq.fill(parameter.issueSlotNum)(Module(new IssueSlot(parameter)))
  val slotsIO = VecInit(slots.map(_.io))

  slotsIO.foreach { slot =>
    slot.wakeUpReq := io.wakeUpReq
  }

  val maxShift = parameter.dispatchWidth
  val vacants = slotsIO.map(!_.valid) ++ io.disUops.map(!_.valid)

  def getShamtOH(countOH: UInt, inc: Bool): UInt = {
    val next = Wire(UInt(maxShift.W))
    next := countOH
    when (countOH === 0.U && inc) {
      next := 1.U
    }
    .elsewhen (!countOH(maxShift - 1) && inc) {
      next := (countOH << 1.U)
    }
    next
  }

  val shamtOH = vacants.scanLeft(0.U)(getShamtOH)

  val stillBeValid = slotsIO.map(_.stillBeValid) ++ io.disUops.map(_.valid)
  val uops = slotsIO.map(_.outUop) ++ io.disUops.map(_.bits)
  val selIssuePort = slotsIO.map(_.outSelIssuePort) ++ Seq.tabulate(parameter.dispatchWidth)(i => (1 << i).U(parameter.issueWidth.W).asBools)

  for (i <- 0 until parameter.issueSlotNum) {
    slotsIO(i).inUop.valid := false.B
    slotsIO(i).inUop.bits := uops(i + 1)
    slotsIO(i).inSelIssuePort := selIssuePort(i + 1)

    for (j <- 1 to maxShift) {
      when (shamtOH(i + j) === (1 << (j - 1)).U) {
        slotsIO(i).inUop.valid := stillBeValid(i + j)
        slotsIO(i).inUop.bits := uops(i + j)
        slotsIO(i).inSelIssuePort := selIssuePort(i + j)
      }
    }
    slotsIO(i).clear := shamtOH(i) =/= 0.U
  }

  var grants = Seq.fill(parameter.issueSlotNum)(false.B)
  io.issuePacket.zipWithIndex.foreach { case (packet, idx) =>
    val requests = slotsIO.map(_.request(idx))
    val selectOH = PriorityEncoderOH(requests)
    packet.valid := requests.reduce(_ || _)
    packet.bits := Mux1H(selectOH, slotsIO.map(_.issueUop))
    grants = selectOH.zip(grants).map { case (s, g) => s || g }
  }
  slotsIO.zip(grants).map { case (s, g) => s.grant := g }

  val willBeAvailable = slotsIO.map { i => (!i.stillBeValid || i.clear) && !i.inUop.valid }
  val numAvailable = PopCount(willBeAvailable)
  io.disUops.zipWithIndex.foreach { case (uop, idx) => uop.ready := RegNext(numAvailable > idx.U) }
}