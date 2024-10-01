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

case class GroomParameter(
  instructionSets:        Set[String],
  flushOnFenceI:          Boolean,
  mulUnroll:              Int,
) {
  // def icacheParameter: ICacheParameter = ICacheParameter(
  //   fetchBytes = 16,
  //   nSets      = 64,
  //   nWays      = 8,
  //   paddrBits  = 32,
  //   blockBytes = 64,
  //   busBytes   = 4
  // )

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

  def coreInstBytes = 32 / 8

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

  val decoderParameter = DecoderParameter(
    instructionSets,
    pipelinedMul,
    flushOnFenceI,
    // todo: default = 16?
    minFLen.getOrElse(16),
    xLen
  )

  def usingMulDiv = hasInstructionSet("rv_m") || hasInstructionSet("rv64_m")
}

class Groom(parameter: GroomParameter) extends Module {

  val decoder: Instance[Decoder] = Instantiate(new Decoder(parameter.decoderParameter))
}
