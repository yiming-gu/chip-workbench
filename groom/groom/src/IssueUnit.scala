package groom.rtl.backend

import chisel3._
import chisel3.util._
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.experimental.hierarchy.{instantiable, public}
import groom._
import org.chipsalliance.rocketv._

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
  val vacants = slotsIO.map(_.valid).map(!_.asBool) ++ io.disUops.map(_.valid).map(!_.asBool)

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

  for (i <- 0 until parameter.issueSlotNum) {
    slotsIO(i).inUop.valid := false.B
    slotsIO(i).inUop.bits := uops(i + 1)

    for (j <- 1 to maxShift) {
      when (shamtOH(i + j) === (1 << (j - 1)).U) {
        slotsIO(i).inUop.valid := stillBeValid(i + j)
        slotsIO(i).inUop.bits := uops(i + j)
      }
    }
    slotsIO(i).clear := shamtOH(i) =/= 0.U
  }

  var requests = slotsIO.map(_.request)
  var grants = Seq.fill(parameter.issueSlotNum)(false.B)
  for (i <- 0 until parameter.issueWidth) {
    val selectOH = PriorityEncoderOH(requests)
    io.issuePacket(i).valid := VecInit(requests).asUInt.orR
    io.issuePacket(i).bits := Mux1H(selectOH, slotsIO.map(_.issueUop))
    requests = requests.zip(selectOH).map { case (req, sel) => req && !sel }
    grants = if (i == 0) {
      requests
    } else {
      requests.zip(grants).map { case (r, g) => r || g }
    }
  }
  slotsIO.zip(grants).map { case (s, g) => s.grant := g }

  io.disUops.zipWithIndex.map { case (disUop, i) =>
    disUop.ready := !slotsIO(parameter.issueSlotNum-1-i).valid
  }

}