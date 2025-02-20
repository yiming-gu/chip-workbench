import chisel3._
import chisel3.experimental.hierarchy.{instantiable, Instance, Instantiate}
import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
import chisel3.probe.{define, Probe, ProbeValue}
import chisel3.util.circt.ClockGate
import chisel3.util.experimental.decode.DecodeBundle
import chisel3.util.{
  log2Ceil,
  log2Up,
  BitPat,
  Cat,
  DecoupledIO,
  Fill,
  MuxLookup,
  PriorityEncoder,
  PriorityMux,
  Queue,
  RegEnable,
  Valid
}
import org.chipsalliance.rocketv.{Decoder, DecoderParameter}
import org.chipsalliance.rocketv.rvdecoderdbcompat.Causes
import org.chipsalliance.rvdecoderdb.Instruction
import org.chipsalliance.rocketv.DecoderInterface
import groom.rtl.frontend._
import groom.rtl.backend._

object GroomParameter {
  implicit def rwP: upickle.default.ReadWriter[GroomParameter] = upickle.default.macroRW[GroomParameter]
}

case class GroomParameter(
  instructionSets:        Set[String],
  flushOnFenceI:          Boolean,
  mulUnroll:              Int,
  paddrBits:              Int,
  cacheBlockBytes:        Int,
  dcacheNSets:            Int,
  renameWidth:            Int,
  pregNum:                Int,
) extends SerializableModuleParameter {

  def usingVector = hasInstructionSet("rv_v")

  // fixed for now
  def usingRVE = false
  def usingDataScratchpad: Boolean = false
  def hasDataECC:          Boolean = false
  def vmidBits      = 0
  def nPerfCounters = 0

  // calculated
  def lgNXRegs = if (usingRVE) 4 else 5

  def pipelinedMul: Boolean = usingMulDiv && mulUnroll == xLen

  def instructions: Seq[Instruction] =
    org.chipsalliance.rvdecoderdb
      .instructions(
        org.chipsalliance.rvdecoderdb.extractResource(getClass.getClassLoader)
      )
      .filter(instruction =>
        (
          instructionSets ++
            // Four mandatory instruction sets.
            Seq("rv_i", "rv_zicsr", "rv_zifencei", "rv_system")
        ).contains(instruction.instructionSet.name)
      )
      .toSeq
      .filter {
        // special case for rv32 pseudo from rv64
        case i if i.pseudoFrom.isDefined && Seq("slli", "srli", "srai").contains(i.name) => true
        case i if i.pseudoFrom.isDefined                                                 => false
        case _                                                                           => true
      }
      .sortBy(i => (i.instructionSet.name, i.name))

  def coreInstBytes = (if (usingCompressed) 16 else 32) / 8

  private def hasInstructionSet(setName: String): Boolean =
    instructions.flatMap(_.instructionSets.map(_.name)).contains(setName)

  private def hasInstruction(instName: String): Boolean = instructions.map(_.name).contains(instName)

  def xLen: Int =
    (hasInstructionSet("rv32_i"), hasInstructionSet("rv64_i")) match {
      case (true, true)   => throw new Exception("cannot support both rv32 and rv64 together")
      case (true, false)  => 32
      case (false, true)  => 64
      case (false, false) => throw new Exception("no basic instruction found.")
    }

  def fLen: Option[Int] =
    (
      hasInstructionSet("rv_f") || hasInstructionSet("rv64_f"),
      hasInstructionSet("rv_d") || hasInstructionSet("rv64_d")
    ) match {
      case (false, false) => None
      case (true, false)  => Some(32)
      case (false, true)  => Some(64)
      case (true, true)   => Some(64)
    }

  def minFLen: Option[Int] =
    if (hasInstructionSet("rv_zfh") || hasInstructionSet("rv64_zfh") || hasInstructionSet("rv_d_zfh"))
      Some(16)
    else
      fLen

  def usingMulDiv = hasInstructionSet("rv_m") || hasInstructionSet("rv64_m")

  def usingAtomics = hasInstructionSet("rv_a") || hasInstructionSet("rv64_a")

  def usingVM = hasInstructionSet("sfence.vma")

  def usingSupervisor = hasInstruction("sret")

  // static to false for now
  def usingHypervisor = hasInstructionSet("rv_h") || hasInstructionSet("rv64_h")

  def usingDebug = hasInstructionSet("rv_sdext")

  def usingCompressed = hasInstructionSet("rv_c")

  def usingFPU = fLen.isDefined

  // static to false for now
  def haveCease = hasInstruction("cease")

  // static to false for now
  def usingNMI = hasInstructionSet("rv_smrnmi")

  // calculated parameter
  def fetchWidth: Int = if (usingCompressed) 2 else 1

  def resetVectorLen: Int = {
    val externalLen = paddrBits
    require(externalLen <= xLen, s"External reset vector length ($externalLen) must be <= XLEN ($xLen)")
    require(
      externalLen <= vaddrBitsExtended,
      s"External reset vector length ($externalLen) must be <= virtual address bit width ($vaddrBitsExtended)"
    )
    externalLen
  }

  val nLocalInterrupts: Int = 0

  def pgIdxBits:                  Int = 12
  def pgLevels:                   Int = if (xLen == 64) 3 /* Sv39 */ else 2 /* Sv32 */
  def pgLevelBits:                Int = 10 - log2Ceil(xLen / 32)
  def maxSVAddrBits:              Int = pgIdxBits + pgLevels * pgLevelBits
  def maxHypervisorExtraAddrBits: Int = 2
  def hypervisorExtraAddrBits:    Int = if (usingHypervisor) maxHypervisorExtraAddrBits else 0
  def maxHVAddrBits:              Int = maxSVAddrBits + hypervisorExtraAddrBits
  def vaddrBits:                  Int = if (usingVM) {
    val v = maxHVAddrBits
    require(v == xLen || xLen > v && v > paddrBits)
    v
  } else {
    // since virtual addresses sign-extend but physical addresses
    // zero-extend, make room for a zero sign bit for physical addresses
    (paddrBits + 1).min(xLen)
  }
  def vpnBits:                    Int = vaddrBits - pgIdxBits
  def ppnBits:                    Int = paddrBits - pgIdxBits
  def vpnBitsExtended:            Int = vpnBits + (if (vaddrBits < xLen) (if (usingHypervisor) 1 else 0) + 1 else 0)

  def vaddrBitsExtended: Int         = vpnBitsExtended + pgIdxBits
  // btb entries
  def btbEntries:        Int         = 28
  def bhtHistoryLength:  Option[Int] = Some(8)
  def bhtCounterLength:  Option[Int] = Some(1)
  def coreInstBits:      Int         = if (usingCompressed) 16 else 32
  def coreMaxAddrBits:   Int         = paddrBits.max(vaddrBitsExtended)
  def lgCacheBlockBytes: Int         = log2Ceil(cacheBlockBytes)
  def blockOffBits = lgCacheBlockBytes
  // todo: 64 -> dcacheParan.nset
  def idxBits:              Int     = log2Ceil(dcacheNSets)
  // dCache untage bits
  def untagBits:            Int     = blockOffBits + idxBits
  def dcacheReqTagBits:     Int     = 6
  def dcacheArbPorts:       Int     = 1 + (if (usingVM) 1 else 0) + (if (usingDataScratchpad) 1 else 0)
  def coreDataBits:         Int     = xLen.max(fLen.getOrElse(0))
  def coreDataBytes:        Int     = coreDataBits / 8
  def separateUncachedResp: Boolean = false
  def minPgLevels:          Int     = {
    val res = xLen match {
      case 32 => 2
      case 64 => 3
    }
    require(pgLevels >= res)
    res
  }

  def maxPAddrBits: Int = {
    require(xLen == 32 || xLen == 64, s"Only XLENs of 32 or 64 are supported, but got $xLen")
    xLen match { case 32 => 34; case 64 => 56 }
  }

  val decoderParameter = DecoderParameter(
    instructionSets,
    pipelinedMul,
    flushOnFenceI,
    // todo: default = 16?
    minFLen.getOrElse(16),
    xLen
  )

  val frontendParameter: FrontendParameter = FrontendParameter(
    fetchBytes = 16,
    instBytes = 4,
    nSets = 64,
    nWays = 8,
    paddrBits = paddrBits,
    blockBytes = 64,
    busBytes = 4,
    decodeWidth = 3,
    fetchBufferEntries = 9,
  )

  val freelistParameter: FreeListParameter = FreeListParameter(
    renameWidth,
    pregNum,
  )
}

class GroomInterface(parameter: GroomParameter) extends Bundle {
  val frontendIo = new FrontendInterface(parameter.frontendParameter)
  // val decoderIo = new DecoderInterface(parameter.decoderParameter)
}

class Groom(val parameter: GroomParameter) extends Module with SerializableModule[GroomParameter] {

  val io = IO(new GroomInterface(parameter))
  val frontend: Instance[Frontend] = Instantiate(new Frontend(parameter.frontendParameter))
  // val decoder: Instance[Decoder] = Instantiate(new Decoder(parameter.decoderParameter))

  frontend.io <> io.frontendIo
  // decoder.io <> io.decoderIo
}
