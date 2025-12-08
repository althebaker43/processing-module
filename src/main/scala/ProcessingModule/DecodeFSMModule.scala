package ProcessingModule

import chisel3._
import chisel3.util.{switch, is}

class WriteRFData(dataWidth : Int, idxWidth : Int) extends Bundle {
  val word = UInt(dataWidth.W)
  val index = UInt(idxWidth.W)
  val valid = Bool()
  val preTrap = Bool()
  override def cloneType = (new WriteRFData(dataWidth, idxWidth)).asInstanceOf[this.type]
}

class DecodeFSMModule(
  iWidth : Int,
  pcWidth : Int,
  instrs : Instructions,
  numOps : Int,
  opWidth : Int,
  rfWidth : Int,
  rfDepth : Int,
  preTrapVector : UInt,
  val dbgMsg : Boolean = false
) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle {
    val instrIn = Flipped(util.Decoupled(new Instruction(iWidth, pcWidth)))
    val data = Input(new WriteRFData(rfWidth, rfIdxWidth))
    val exData = Input(new WriteRFData(rfWidth, rfIdxWidth))
    val instrValids = Output(Vec(numInstrs, Bool()))
    val ops = Output(Vec(numOps, UInt(opWidth.W)))
    val branchPC = util.Valid(SInt(pcWidth.W))
    val instrOut = Output(new Instruction(iWidth, pcWidth))
    val preTrapInstr = Output(Bool())
    val instrReady = Input(Bool())
  })

  val stateInit :: stateReady :: stateWait :: stateWaitBranch :: stateFlush :: Nil = util.Enum(5)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val initCountReg = RegInit(0.U(rfIdxWidth.W))

  val preTrapEnabledReg = RegInit(false.B)
  io.preTrapInstr := preTrapEnabledReg

  val hazard = Wire(Bool())
  val branchTaken = Wire(Bool())
  val branchTakenReg = RegInit(false.B)
  val branchFinished = Wire(Bool())
  val branchPCReg = Reg(util.Valid(UInt(pcWidth.W)))
  branchFinished := branchPCReg.bits === io.instrIn.bits.pc
  val stall = Wire(Bool())
  stall := hazard | ((branchTaken | branchTakenReg) & !branchFinished)
  when (branchTaken & (stateReg =/= stateWaitBranch)) {
    branchTakenReg := true.B
  } .elsewhen (branchFinished) {
    branchTakenReg := false.B
    branchPCReg.valid := false.B
    preTrapEnabledReg := false.B
  }
  when (stateReg === stateInit) {
    branchPCReg.valid := false.B
  }

  val hazardInstrReg = Reg(util.Valid(new Instruction(iWidth, pcWidth)))
  when (hazard & io.instrIn.valid & (stateReg === stateReady)) {
    hazardInstrReg.valid := true.B
    hazardInstrReg.bits := io.instrIn.bits
  } .elsewhen ((stateReg === stateFlush) | (stateReg === stateWaitBranch)) {
    hazardInstrReg.valid := false.B
  }

  def printDbg(fmt : String, data : Bits*) : Unit = {
    if (dbgMsg) printf(Printable.pack(fmt, data:_*))
  }

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printDbg("Decode state = init\n")
      printDbg("Clearing RF at %d\n", initCountReg)
      when ((initCountReg === rfDepth.U) | initCountReg.andR()) {
        nextState := stateReady
      }
    }

    is (stateReady) {
      printDbg("Decode state = ready\n")
      when (hazard | !io.instrReady) {
        nextState := stateWait
      } .elsewhen (branchTaken) {
        nextState := stateWaitBranch
      }
    }

    is (stateWait) {
      printDbg("Decode state = wait\n")
      when (!hazard & io.instrReady) {
        when (branchTaken | branchTakenReg) {
          nextState := stateWaitBranch
        } .otherwise {
          when (hazardInstrReg.valid) {
            nextState := stateFlush
          } .otherwise {
            nextState := stateReady
          }
        }
      }
    }

    is (stateWaitBranch) {
      printDbg("Decode state = waitBr\n")
      when (branchFinished) {
        when (!hazard & io.instrReady) {
          nextState := stateReady
        } .otherwise {
          nextState := stateWait
        }
      }
    }

    is (stateFlush) {
      printDbg("Decode state = flush\n")
      nextState := stateReady
    }
  }

  val rf = Mem(rfDepth, new RFWord(rfWidth))

  // TODO: add parameter for preTrapRF depth
  val preTrapRF = Mem(4, new RFWord(rfWidth))

  val rfDataIn = Wire(new RFWord(rfWidth))
  rfDataIn.word := 0.U
  rfDataIn.isWritten := true.B

  val rfExDataIn = Wire(new RFWord(rfWidth))
  rfExDataIn.word := 0.U
  rfExDataIn.isWritten := true.B

  val rfInstrDataIn = Wire(new RFWord(rfWidth))
  rfInstrDataIn.word := 0.U
  rfInstrDataIn.isWritten := true.B

  when (stateReg === stateInit) {
    rf.write(initCountReg, rfDataIn)
    when (initCountReg < 4.U) {
      // TODO: add parameter for preTrapRF depth
      preTrapRF.write(initCountReg, rfDataIn)
    }
    initCountReg := initCountReg + 1.U
  } .elsewhen (initCountReg =/= 0.U) {
    initCountReg := 0.U
  } .otherwise {

    when (io.data.valid) {
      rfDataIn.word := io.data.word
      rfDataIn.isWritten := true.B
      when (io.data.preTrap) {
        preTrapRF.write(io.data.index, rfDataIn)
      } .otherwise {
        rf.write(io.data.index, rfDataIn)
      }
    }

    when (io.exData.valid) {
      rfExDataIn.word := io.exData.word
      rfExDataIn.isWritten := true.B
      when (io.exData.preTrap) {
        preTrapRF.write(io.exData.index, rfExDataIn)
      } .otherwise {
        rf.write(io.exData.index, rfExDataIn)
      }
    }
  }

  val instrReg = Reg(util.Valid(new Instruction(iWidth, pcWidth)))
  when (!stall) {
    when (((stateReg === stateReady) | (branchTakenReg & branchFinished)) & io.instrIn.valid) {
      instrReg.bits := io.instrIn.bits
      instrReg.valid := io.instrIn.valid
    } .elsewhen (stateReg === stateFlush) {
      instrReg := hazardInstrReg
    } .elsewhen (stateReg =/= stateWait) {
      instrReg.valid := false.B
    }
  }

  io.instrIn.ready := (stateReg === stateReady) | (stateReg === stateWaitBranch)
  io.branchPC.valid := false.B
  io.branchPC.bits := 0.S
  io.instrOut := instrReg.bits

  val instrValids = Wire(Vec(numInstrs, Bool()))
  io.instrValids := instrValids
  when (stall) {
    for (idx <- 0 until instrs.logic.size) {
      io.instrValids(idx) := false.B
    }
  }

  val ops = Wire(Vec(numOps, UInt(opWidth.W)))
  io.ops := ops
  for (idx <- 0 until numOps) {
    ops(idx) := 0.U
  }

  hazard := false.B
  branchTaken := false.B
  for ((instr, idx) <-  instrs.logic.zipWithIndex) {

    when (instrReg.valid) {
      instrValids(idx) := instr.decode(instrReg.bits.word)
    } .otherwise {
      instrValids(idx) := false.B
    }

    when (instrValids(idx)) {

      for (opIdx <- 0 until instr.numOps) {

        val rfIdx = Wire(UInt(rfIdxWidth.W))
        // val writeRF = Wire(Bool())
        // val writeRFIdx = Wire(UInt(rfIdxWidth.W))

        rfIdx := instr.getRFIndex(instrReg.bits.word, opIdx)
        // writeRF := instr.writeRF(instrReg.bits.word)
        // writeRFIdx := instr.getWriteIndex(instrReg.bits.word, ops)

        when (io.data.valid & (rfIdx === io.data.index)) {
          ops(opIdx) := io.data.word
        } .elsewhen (io.exData.valid & (rfIdx === io.exData.index)) {
          ops(opIdx) := io.exData.word
        } .otherwise {
          when (!preTrapEnabledReg) {
            when (rf(rfIdx).isWritten) {
              ops(opIdx) := rf(rfIdx).word
            } .otherwise {
              hazard := true.B
            }
          } .otherwise {
            when (preTrapRF(rfIdx).isWritten) {
              ops(opIdx) := preTrapRF(rfIdx).word
            } .otherwise {
              hazard := true.B
            }
          }
        }
      }

      when (!branchFinished) {
        when (instr.raiseException(instrReg.bits.word, ops)) {
          io.branchPC.valid := !hazard & !branchPCReg.valid
          io.branchPC.bits := preTrapVector.asSInt()
          branchTaken := true.B
          branchPCReg.bits := io.branchPC.bits.asUInt
          branchPCReg.valid := !hazard
          preTrapEnabledReg := true.B
        } .elsewhen (instr.branch()) {
          when (instr.relativeBranch()) {
            io.branchPC.bits := instr.getBranchPC(instrReg.bits.word, ops) + instrReg.bits.pc.asSInt()
          } .otherwise {
            io.branchPC.bits := instr.getBranchPC(instrReg.bits.word, ops)
          }
          when (io.branchPC.bits.asUInt =/= (instrReg.bits.pc + 4.U)) {
            io.branchPC.valid := !hazard & !branchPCReg.valid
            branchTaken := true.B
            branchPCReg.bits := io.branchPC.bits.asUInt
            branchPCReg.valid := !hazard
          }
        }
      }
      when (instr.writeRF(instrReg.bits.word)) {
        when (io.data.valid & (instr.getWriteIndex(instrReg.bits.word, ops) === io.data.index)) {
          rfDataIn.isWritten := false.B
        } .elsewhen (io.exData.valid & (instr.getWriteIndex(instrReg.bits.word, ops) === io.exData.index)) {
          rfExDataIn.isWritten := false.B
        } .otherwise {
          rfInstrDataIn.isWritten := false.B
          when (preTrapEnabledReg) {
            preTrapRF.write(instr.getWriteIndex(instrReg.bits.word, ops), rfInstrDataIn)
          } .otherwise {
            rf.write(instr.getWriteIndex(instrReg.bits.word, ops), rfInstrDataIn)
          }
        }
      }
    }
  }

  val isInstrValid = Wire(Bool())
  isInstrValid := false.B
  when (((stateReg === stateReady) | (stateReg === stateFlush)) & instrReg.valid) {
    for ((instr, idx) <- instrs.logic.zipWithIndex) {
      when (instrValids(idx)) {
        isInstrValid := true.B
      }
    }
    when (!isInstrValid) {
      // TODO: raise exception
      printDbg("Invalid instruction %x at address %x\n", instrReg.bits.word, instrReg.bits.pc)
    }
  }
}
