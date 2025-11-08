// package groom.rtl.backend

// import chisel3._
// import chisel3.util._
// import chisel3.experimental.{SerializableModule, SerializableModuleParameter}
// import chisel3.experimental.hierarchy.{instantiable, public}
// import org.chipsalliance.rocketv.DecoderParameter
// import org.chipsalliance.t1.rtl._
// import groom._

// object StoreQueueParameter {
//   implicit def rwP: upickle.default.ReadWriter[StoreQueueParameter] = upickle.default.macroRW[StoreQueueParameter]
// }

// case class StoreQueueParameter(
//   xLen: Int,
//   dispatchWidth: Int,
//   commitWidth: Int,
//   sqNum: Int,
//   robNum: Int,
//   paddrBits: Int,
//   lregNum: Int,
//   pregNum: Int,
//   writeBackWidth: Int,
//   decoderParameter: DecoderParameter,
// ) extends SerializableModuleParameter {
//   val sqSize = log2Ceil(sqNum)
//   val robSize = log2Ceil(robNum)
//   val lregSize = log2Ceil(lregNum)
//   val pregSize = log2Ceil(pregNum)
// }

// class StoreQueueInterface(parameter: StoreQueueParameter) extends Bundle {
//   val reset = Input(Bool())
//   // enq
//   val enq = Flipped(DecoupledIO(Vec(parameter.dispatchWidth, new MicroOp(
//     parameter.paddrBits,
//     parameter.decoderParameter,
//     parameter.pregSize,
//     parameter.robSize
//   ))))

//   val wb = Flipped(ValidIO(new LqWb(parameter.sqSize, parameter.xLen)))

//   val sqIdx = Vec(parameter.dispatchWidth, Output(UInt(parameter.sqSize.W)))
// }

// @instantiable
// class StoreQueue(val parameter: StoreQueueParameter)
//     extends Module
//     with SerializableModule[StoreQueueParameter] {

//   @public
//   val io = IO(new StoreQueueInterface(parameter))

//   val sqEntries = Reg(Vec(parameter.sqNum, new LqEntry(parameter.robSize, parameter.xLen)))
//   val allocated = RegInit(VecInit(Seq.fill(parameter.sqNum)(false.B)))
//   val writebacked = RegInit(VecInit(Seq.fill(parameter.sqNum)(false.B)))

//   val sqDeqPOH = RegInit(1.U(parameter.sqNum.W))
//   val sqDeqFlag = RegInit(false.B)
//   val sqEnqPOH = RegInit(1.U(parameter.sqNum.W))
//   val sqEnqFlag = RegInit(false.B)

//   val sqEnqPOHShift = CircularShift(sqEnqPOH)
//   val sqEnqPOHVec = VecInit.tabulate(parameter.dispatchWidth)(sqEnqPOHShift.left)
//   val sqEnqPVec = VecInit(sqEnqPOHVec.map(OHToUInt(_)))

//   val flush = Wire(Bool())

//   when (flush) {
//     for (i <- 0 until parameter.sqNum) {
//       allocated(i) := false.B
//     }
//   }

//   when (io.enq.fire) {
//     for (i <- 0 until parameter.dispatchWidth) {
//       val idx = sqEnqPVec(i)
//       allocated(idx) := true.B
//     }
//   }

//   when (io.wb.valid) {
//     sqEntries(io.wb.bits.lqIdx).addr := io.wb.bits.addr
//     writebacked(io.wb.bits.lqIdx) := io.wb.bits.dataValid
//   }

//   val lqEnqPOHNext = sqEnqPOHShift.left(parameter.dispatchWidth)
//   when (io.enq.fire) {
//     sqEnqPOH := lqEnqPOHNext
//     when (sqEnqPOH(parameter.sqNum - 1,
//                    parameter.sqNum - parameter.dispatchWidth).orR &&
//           lqEnqPOHNext(parameter.dispatchWidth - 1, 0).orR) {
//       sqEnqFlag := !sqEnqFlag
//     }
//   }
//   .elsewhen (flush) {
//     sqEnqPOH := 1.U
//     sqEnqFlag := false.B
//   }

//   val sqDeqPOHShift = CircularShift(sqDeqPOH)
//   val sqDeqPOHVec = VecInit.tabulate(parameter.commitWidth)(sqDeqPOHShift.left)

//   val commitCandidate = Mux1H(sqDeqPOH, sqEntries)
//   val commitAllocated = Mux1H(sqDeqPOH, allocated)
//   val commitWritebacked = Mux1H(sqDeqPOH, writebacked)

//   val commitValid = commitAllocated && commitWritebacked
//   val allCommitted = commitValid

//   val sqDeqPOHNext = sqDeqPOHShift.left(parameter.commitWidth)
//   when (allCommitted) {
//     sqDeqPOH := sqDeqPOHNext
//     when (sqDeqPOH(parameter.sqNum - 1,
//                    parameter.sqNum - parameter.commitWidth).orR &&
//           sqDeqPOHNext(parameter.commitWidth - 1, 0).orR) {
//       sqDeqFlag := !sqDeqFlag
//     }
//   }
//   .elsewhen (flush) {
//     sqDeqPOH := 1.U
//     sqDeqFlag := false.B
//   }

//   val empty = sqEnqPOH === sqDeqPOH && sqEnqFlag === sqDeqFlag
//   io.sqIdx := sqEnqPVec

//   val willFull = sqEnqPOHVec.drop(1).map(p => p === sqDeqPOH).reduce(_ || _)
//   io.enq.ready := !willFull && !flush
// }
