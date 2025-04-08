package groom

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
import org.chipsalliance.amba.axi4.bundle.{AXI4BundleParameter, AXI4ROIrrevocable, AXI4RWIrrevocable}
import groom.rtl.frontend._
import groom.rtl.backend._
import spire.std.int

object GroomParameter {
  implicit def rwP: upickle.default.ReadWriter[GroomParameter] = upickle.default.macroRW[GroomParameter]
}

case class MicroOpParameter(
  val frontendParameter: FrontendParameter,
  val decoderParameter: DecoderParameter,
  val renameParameter: RenameParameter,
) extends SerializableModuleParameter {

}

case class GroomParameter(
  instructionSets:        Set[String],
  flushOnFenceI:          Boolean,
  mulUnroll:              Int,
  paddrBits:              Int,
  cacheBlockBytes:        Int,
  dcacheNSets:            Int,
  decodeWidth:            Int,
  renameWidth:            Int,
  pregNum:                Int,
  instBytes:              Int,
  robNum:                 Int,
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

  val renameParameter: RenameParameter = RenameParameter(
    renameWidth = renameWidth,
    commitWidth = 1,
    pregNum = 128,
    lregNum = 32,
    instBytes = 4,
    paddrBits = 32,
    wakeUpWidth = 1,
    delaySize = 3,
  )

  val intIssueParameter: IssueParameter = IssueParameter(
    paddrBits = paddrBits,
    dispatchWidth = 3,
    issueSlotNum = 10,
    wakeUpWidth = 1,
    issueWidth = 1,
    pregNum = 128,
    delayWidth = 3,
    robNum = robNum,
    decoderParameter = decoderParameter,
  )

  val dispatchParameter: DispatchParameter = DispatchParameter(
    renameWidth = renameWidth,
    dispatchWidth = 3,
    issueQueueNum = 1,
  )

  val regFileParameter: RegFileParameter = RegFileParameter(
    xLen = xLen,
    paddrBits = paddrBits,
    intIssueWidth = 1,
    memIssueWidth = 0,
    pregNum = 128,
    uopSize = 4,
    robNum = robNum,
    decoderParameter = decoderParameter,
  )

  val execUnitParameter: ExecUnitParameter = ExecUnitParameter(
    xLen = xLen,
    uopSize = 4,
    intIssueWidth = 1,
    paddrBits = 32,
    pregNum = 128,
    robNum = 32,
    decoderParameter = decoderParameter,
  )

  val microOpParameter: MicroOpParameter = MicroOpParameter(
    frontendParameter,
    decoderParameter,
    renameParameter,
  )

  val robParameter: ROBParameter = ROBParameter(
    dispatchWidth = 3,
    commitWidth = 1,
    robNum = 32,
    paddrBits = 32,
    lregNum = 32,
    pregNum = 128,
    renameWidth = 3,
    writebackWidth = 1,
    decoderParameter = decoderParameter,
  )

  val pregSize = log2Ceil(pregNum)
  val robSize  = log2Ceil(robNum)
}

class GroomInterface(parameter: GroomParameter) extends Bundle {
  val instructionFetchAXI: AXI4ROIrrevocable =
    org.chipsalliance.amba.axi4.bundle.AXI4ROIrrevocable(parameter.frontendParameter.iCacheParameter.instructionFetchParameter)
}

class Groom(val parameter: GroomParameter) extends Module with SerializableModule[GroomParameter] {

  val io = IO(new GroomInterface(parameter))
  val frontend: Instance[Frontend] = Instantiate(new Frontend(parameter.frontendParameter))

  // val decoder = Seq.fill(parameter.decodeWidth)(Instantiate(new Decoder(parameter.decoderParameter)))
  val uops = Wire(Vec(parameter.decodeWidth, parameter.decoderParameter.table.bundle))

  frontend.io.frontendPacket.bits.fetchPacket.zip(uops).foreach { case (f, u) =>
    val decoder = Instantiate(new Decoder(parameter.decoderParameter))
    decoder.io.instruction := f.bits.inst
    u := decoder.io.output
  }

  val rename: Instance[Rename] = Instantiate(new Rename(parameter.renameParameter))
  val renamePacket = Wire(Vec(parameter.renameWidth, Valid(new MicroOp(
    parameter.paddrBits,
    parameter.decoderParameter,
    parameter.pregSize,
    parameter.robSize
  ))))
  renamePacket.zip(frontend.io.frontendPacket.bits.fetchPacket).zip(uops).foreach { case ((r, f), u) =>
    r.valid := RegEnable(f.valid, false.B, frontend.io.frontendPacket.fire)
    r.bits.fetchPacket := RegEnable(f.bits, 0.U.asTypeOf(new FetchPacket(parameter.paddrBits)), frontend.io.frontendPacket.fire)
    r.bits.uop := RegEnable(u, 0.U.asTypeOf(parameter.decoderParameter.table.bundle), frontend.io.frontendPacket.fire)
  }

  rename.io.decodeLreg.valid := frontend.io.frontendPacket.valid
  frontend.io.frontendPacket.ready := rename.io.decodeLreg.ready

  rename.io.decodeLreg.bits.zip(rename.io.renamePreg.bits).zip(renamePacket).foreach { case ((lreg, preg), r) =>
    lreg.lsrcValid(0) := r.bits.uop(parameter.decoderParameter.rxs1)
    lreg.lsrcValid(1) := r.bits.uop(parameter.decoderParameter.rxs2)
    lreg.lsrc(0) := r.bits.fetchPacket.inst(19, 15)
    lreg.lsrc(1) := r.bits.fetchPacket.inst(24, 20)
    lreg.ldstValid := r.bits.uop(parameter.decoderParameter.wxd)
    lreg.ldst := r.bits.fetchPacket.inst(11, 7)
    r.bits.preg := preg
  }
  rename.io.redirect := false.B

  val dispatch: Instance[Dispatch] = Instantiate(new Dispatch(parameter.dispatchParameter))
  dispatch.io.renameVld.valid := rename.io.renamePreg.valid
  rename.io.renamePreg.ready := dispatch.io.renameVld.ready
  dispatch.io.renameVld.bits.zip(renamePacket).foreach { case (d, r) =>
    d.instType := r.bits.uop(parameter.decoderParameter.mem)
  }

  val intIssueUnit: Instance[IssueUnit] = Instantiate(new IssueUnit(parameter.intIssueParameter))
  intIssueUnit.io.disUops.zip(dispatch.io.intIqVld).zip(dispatch.io.intIqRdy).zip(renamePacket).foreach { case (((d, v), r), p) =>
    d.valid := v
    r := d.ready
    d.bits := p.bits
  }
  intIssueUnit.io.wakeUpReq.zip(intIssueUnit.io.issuePacket).foreach { case (w, i) =>
    w.valid := i.valid && i.bits.uop(parameter.decoderParameter.wxd)
    w.bits.pdst := i.bits.preg.pdst
    w.bits.delay := 0.U
  }
  rename.io.wakeUpReq.zip(intIssueUnit.io.issuePacket).foreach { case (w, i) =>
    w.valid := i.valid && i.bits.uop(parameter.decoderParameter.wxd)
    w.bits.pdst := i.bits.preg.pdst
    w.bits.delay := 0.U
  }

  val regFileStage: Instance[RegFileStage] = Instantiate(new RegFileStage(parameter.regFileParameter))
  regFileStage.io.intIssuePacket := intIssueUnit.io.issuePacket

  val execPacket = RegNext(intIssueUnit.io.issuePacket)

  val execUnit: Instance[ExecutionUnit] = Instantiate(new ExecutionUnit(parameter.execUnitParameter))
  execUnit.io.execUnitReq := regFileStage.io.execUnitReq
  execUnit.io.execPacket := execPacket

  regFileStage.io.execUnitResp := execUnit.io.execUnitResp

  val rob: Instance[ROB] = Instantiate(new ROB(parameter.robParameter))
  rob.io.enq := renamePacket
  renamePacket.zip(rob.io.robIdx).foreach { case (r, idx) =>
    r.bits.robIdx := idx
  }

  rob.io.wb.zip(execUnit.io.execUnitResp).foreach { case (wb, r) =>
    wb.valid := r.valid
    wb.bits := r.bits.robIdx
  }

  rename.io.commit := rob.io.commit

  io.instructionFetchAXI <> frontend.io.instructionFetchAXI
}
