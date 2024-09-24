import chisel3._
import groom.frontend._

case class GroomParameter() {
  def icacheParameter: ICacheParameter = ICacheParameter(
    fetchBytes = 16,
    nSets      = 64,
    nWays      = 8,
    paddrBits  = 32,
    blockBytes = 64,
    busBytes   = 4
  )
}

class Groom(parameter: GroomParameter) extends Module {
  val io = IO(new ICacheInterface(parameter.icacheParameter))
  val iCache = Module(new ICache(parameter.icacheParameter))
  io <> iCache.io
}
