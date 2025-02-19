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

  val stateInit :: stateReady :: stateWait :: stateFlush :: Nil = util.Enum(4)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val initCountReg = RegInit(0.U(rfIdxWidth.W))

  val hazard = Wire(Bool())

  val hazardInstrReg = Reg(util.Valid(new Instruction(iWidth, pcWidth)))
  when (hazard & io.instrIn.valid) {
    hazardInstrReg.valid := true.B
    hazardInstrReg.bits := io.instrIn.bits
  } .elsewhen (stateReg === stateFlush) {
    hazardInstrReg.valid := false.B
  }

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
      when (hazard) {
        nextState := stateWait
      }
    }

    is (stateWait) {
      printf("Decode state = wait\n")
      when (!hazard) {
        when (hazardInstrReg.valid) {
          nextState := stateFlush
        } .otherwise {
          nextState := stateReady
        }
      }
    }

    is (stateFlush) {
      printf("Decode state = flush\n")
      nextState := stateReady
    }
  }

  val rf = Mem(rfDepth, new RFWord(rfWidth))

  val rfDataIn = Wire(new RFWord(rfWidth))
  rfDataIn.word := 0.U
  rfDataIn.isWritten := true.B
  when (io.data.valid) {
    rfDataIn.word := io.data.bits
    rfDataIn.isWritten := true.B
    rf.write(io.index, rfDataIn)
  }

  val rfExDataIn = Wire(new RFWord(rfWidth))
  rfExDataIn.word := 0.U
  rfExDataIn.isWritten := true.B
  when (io.exData.valid) {
    rfExDataIn.word := io.exData.bits
    rfExDataIn.isWritten := true.B
    rf.write(io.exIndex, rfExDataIn)
  }

  val rfInstrDataIn = Wire(new RFWord(rfWidth))
  rfInstrDataIn.word := 0.U
  rfInstrDataIn.isWritten := true.B


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
  } .elsewhen (stateReg === stateFlush) {
    instrReg := hazardInstrReg
  } .elsewhen ((stateReg =/= stateWait) & !hazard) {
    instrReg.valid := false.B
  }

  io.instrIn.ready := (stateReg === stateReady)
  io.branchPC.valid := false.B
  io.branchPC.bits := 0.S
  io.instrOut := instrReg.bits

  val instrValids = Wire(Vec(numInstrs, Bool()))
  io.instrValids := instrValids
  when (hazard) {
    for (idx <- 0 until instrs.logic.size) {
      io.instrValids(idx) := false.B
    }
  }

  val ops = Wire(Vec(numOps, UInt(opWidth.W)))
  io.ops := ops
  for (idx <- 0 until numOps) {
    ops(idx) := 0.U
  }

  hazard := !io.instrReady
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
        when (io.data.valid & (rfIdx === io.index)) {
          ops(opIdx) := io.data.bits
        } .elsewhen (io.exData.valid & (rfIdx === io.exIndex)) {
          ops(opIdx) := io.exData.bits
        } .otherwise {
          when (rf(rfIdx).isWritten) {
            ops(opIdx) := rf(rfIdx).word
          } .otherwise {
            hazard := true.B
          }
        }
      }
      when (instr.branch()) {
        io.branchPC.valid := true.B
        when (instr.relativeBranch()) {
          io.branchPC.bits := instr.getBranchPC(instrReg.bits.word, ops) + instrReg.bits.pc.asSInt()
        } .otherwise {
          io.branchPC.bits := instr.getBranchPC(instrReg.bits.word, ops)
        }
      }
      when (instr.writeRF(instrReg.bits.word)) {
        when (io.data.valid & (instr.getWriteIndex(instrReg.bits.word, ops) === io.index)) {
          rfDataIn.isWritten := false.B
        } .elsewhen (io.exData.valid & (instr.getWriteIndex(instrReg.bits.word, ops) === io.exIndex)) {
          rfExDataIn.isWritten := false.B
        } .otherwise {
          rfInstrDataIn.isWritten := false.B
          rf.write(instr.getWriteIndex(instrReg.bits.word, ops), rfInstrDataIn)
        }
      }
    }
  }
}
