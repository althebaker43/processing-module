package ProcessingModule

import chisel3._
import chisel3.util.{switch, is}

class DecodeFSMModule(
  iWidth : Int,
  pcWidth : Int,
  instrs : Instructions,
  numOps : Int,
  opWidth : Int,
  rfWidth : Int,
  rfDepth : Int
) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle {
    val instrIn = Flipped(util.Decoupled(new Instruction(iWidth, pcWidth)))
    val data = Flipped(util.Valid(UInt(rfWidth.W)))
    val index = Input(UInt(rfIdxWidth.W))
    val exData = Flipped(util.Valid(UInt(rfWidth.W)))
    val exIndex = Input(UInt(rfIdxWidth.W))
    val instrValids = Output(Vec(numInstrs, Bool()))
    val ops = Output(Vec(numOps, UInt(opWidth.W)))
    val branchPC = util.Valid(SInt(pcWidth.W))
    val instrOut = Output(new Instruction(iWidth, pcWidth))
    val instrReady = Input(Bool())
  })

  val stateInit :: stateReady :: Nil = util.Enum(2)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val initCountReg = RegInit(0.U(rfIdxWidth.W))

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printf("Decode state = init\n")
      printf("Clearing RF at %d\n", initCountReg)
      when ((initCountReg === rfDepth.U) | initCountReg.andR()) {
        nextState := stateReady
      }
    }

    is (stateReady) {
      printf("Decode state = ready\n")
    }
  }

  val rf = Mem(rfDepth, new RFWord(rfWidth))

  val rfDataIn = Wire(new RFWord(rfWidth))
  rfDataIn.word := 0.U
  rfDataIn.isWritten := true.B

  val rfExDataIn = Wire(new RFWord(rfWidth))
  rfExDataIn.word := 0.U
  rfExDataIn.isWritten := true.B

  when (stateReg === stateReady) {
    when (io.data.valid) {
      rfDataIn.word := io.data.bits
      rf.write(io.index, rfDataIn)
    }
    when (io.exData.valid) {
      rfExDataIn.word := io.exData.bits
      rf.write(io.exIndex, rfExDataIn)
    }
  }

  when (stateReg === stateInit) {
    rf.write(initCountReg, rfDataIn)
    initCountReg := initCountReg + 1.U
  } .elsewhen (initCountReg =/= 0.U) {
    initCountReg := 0.U
  }

  val instrReg = Reg(util.Valid(new Instruction(iWidth, pcWidth)))
  when ((stateReg === stateReady) & io.instrIn.valid) {
    instrReg.bits := io.instrIn.bits
    instrReg.valid := io.instrIn.valid
  }

  io.instrIn.ready := (stateReg === stateReady)
  for (idx <- 0 until numOps) {
    io.ops(idx) := 0.U
  }
  io.branchPC.valid := false.B
  io.branchPC.bits := 0.S
  io.instrOut := instrReg.bits

  val instrValids = Wire(Vec(numInstrs, Bool()))
  io.instrValids := instrValids

  for ((instr, idx) <-  instrs.logic.zipWithIndex) {
    when ((stateReg === stateReady) & instrReg.valid) {
      instrValids(idx) := instr.decode(instrReg.bits.word)
    } .otherwise {
      instrValids(idx) := false.B
    }
    when (instrValids(idx)) {
      for (opIdx <- 0 until instr.numOps) {
        val rfIdx = Wire(UInt(rfIdxWidth.W))
        rfIdx := instr.getRFIndex(instrReg.bits.word, opIdx)
        io.ops(opIdx) := rf(rfIdx).word
      }
    }
  }
}
