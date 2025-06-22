package groom

import chisel3._
import chisel3.util.{isPow2, log2Ceil, Cat, Decoupled, DecoupledIO, Valid, ValidIO}
import org.chipsalliance.rocketv._

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
  val needFlush   = Bool()
}

class ExuInput(xLen: Int, pregSize: Int, robSize: Int, decoderParameter: DecoderParameter) extends Bundle {
  val dw = UInt(1.W)
  val fn = UInt(decoderParameter.UOPALU.width.W)
  val selAlu1 = UInt(decoderParameter.UOPA1.width.W)
  val selAlu2 = UInt(decoderParameter.UOPA2.width.W)
  val isJal = Bool()
  val isJalr = Bool()
  val isBranch = Bool()
  val pc = UInt(xLen.W)
  val imm = SInt(xLen.W)
  val rsrc = Vec(2, UInt(xLen.W))
  val robIdx = UInt(robSize.W)
  val wxd = Bool()
  val pdst = UInt(pregSize.W)
}

class ExuOutput(xLen: Int, robSize: Int, pregSize: Int) extends Bundle {
  val wxd = Bool()
  val data = UInt(xLen.W)
  val robIdx = UInt(robSize.W)
  val pdst = UInt(pregSize.W)
  val cfiTaken = Bool()
  val dnpc = UInt(xLen.W)
}

class RobEntry(lregSize: Int, pregSize: Int, xLen: Int) extends Bundle {
  val valid = Bool()
  val wxd = Bool()
  val ldst = UInt(lregSize.W)
  val pdst = UInt(pregSize.W)
  val pc = UInt(xLen.W)
  val writebacked = Bool()
  val needFlush = Bool()
  val dnpc = UInt(xLen.W)
}

class RobCommit(commitWidth: Int, lregSize: Int, pregSize: Int) extends Bundle {
  val isCommit = Bool()
  val commitValid = Vec(commitWidth, Bool())
  val isWalk = Bool()
  val walkValid = Vec(commitWidth, Bool())
  val wxd = Vec(commitWidth, Bool())
  val ldst = Vec(commitWidth, UInt(lregSize.W))
  val pdst = Vec(commitWidth, UInt(pregSize.W))
  val pc = Vec(commitWidth, UInt(32.W))
}

class RobWb(xLen: Int, robSize: Int) extends Bundle {
  val robIdx = UInt(robSize.W)
  val needFlush = Bool()
  val dnpc = UInt(xLen.W)
}

class Redirect(xLen: Int) extends Bundle{
  val pc = UInt(xLen.W)
}

class RobIdx(robSize: Int) extends Bundle {
  val robIdx = UInt(robSize.W)
  val robIdxFlag = Bool()
}

class LqEntry(robSize: Int, xLen: Int) extends Bundle {
  val robIdx = UInt(robSize.W)
  val addr = UInt(xLen.W)
}

class LqWb(lqSize: Int, xLen: Int) extends Bundle {
  val lqIdx = UInt(lqSize.W)
  val addr = UInt(xLen.W)
  val dataValid = Bool()
}
