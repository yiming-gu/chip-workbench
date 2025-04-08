package groom

import chisel3._
import chisel3.util.{isPow2, log2Ceil, Cat, Decoupled, DecoupledIO, Valid, ValidIO}
import org.chipsalliance.rocketv._

class ExecUnitReq(xLen: Int, uopSize: Int) extends Bundle {
  val fn = UInt(uopSize.W)
  val op1Data = UInt(xLen.W)
  val op2Data = UInt(xLen.W)
}

class ExecUnitResp(xLen: Int, robSize: Int, pregSize: Int) extends Bundle {
  val wxd = Bool()
  val out = UInt(xLen.W)
  val pdst = UInt(pregSize.W)
  val robIdx = UInt(robSize.W)
}

class WakeUpReq(pregNum: Int, delaySize: Int) extends Bundle {
  val pdst = UInt(log2Ceil(pregNum).W)
  val delay = UInt(delaySize.W)
}

class FetchPacket(paddrBits: Int) extends Bundle {
  val pc:    UInt = UInt(paddrBits.W)
  val inst:  UInt = UInt(32.W)
}

class RenamePreg(pregSize: Int) extends Bundle {
  val psrc = Vec(2, UInt(pregSize.W))
  val pdst = UInt(pregSize.W)
  val psrcBusy = Vec(2, Bool())
}

class MicroOp(paddrBits: Int, decoderParameter: DecoderParameter, pregSize: Int, robSize: Int) extends Bundle {
  val fetchPacket = new FetchPacket(paddrBits)
  val uop         = decoderParameter.table.bundle
  val preg        = new RenamePreg(pregSize)
  val robIdx      = UInt(robSize.W)
}

class RobCommit(lregSize: Int, pregSize: Int) extends Bundle {
  val wxd = Bool()
  val ldst = UInt(lregSize.W)
  val pdst = UInt(pregSize.W)
}
