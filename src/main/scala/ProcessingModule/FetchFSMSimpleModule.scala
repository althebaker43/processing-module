package ProcessingModule


import chisel3._
import chisel3.util.{switch, is}

class FetchFSMSimpleModule(iWidth : Int, pcWidth : Int, pcAlign : Int, val dbgMsg : Boolean = true) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(SInt(pcWidth.W)))
    val pcOut = util.Valid(UInt(pcWidth.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(new Instruction(iWidth, pcWidth))
  })

  val stateInit :: stateInitMem :: stateInitMem2 :: stateValid :: stateFreeze :: stateFreezeOut :: stateFlush :: Nil = util.Enum(7)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val pcReg = RegInit(0.U(pcWidth.W))
  val pcBufReg = Reg(UInt(pcWidth.W))
  val instrReg = Reg(new Instruction(iWidth, pcWidth))

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printDbg("init")
      when (io.instr.ready & !io.branchPCIn.valid) {
        nextState := stateInitMem
      }
    }

    is (stateInitMem) {
      printDbg("initMem")
      when (io.branchPCIn.valid) {
        nextState := stateFlush
      } .otherwise {
        nextState := stateInitMem2
      }
    }

    is (stateInitMem2) {
      printDbg("initMem2")
      when (io.branchPCIn.valid) {
        nextState := stateFlush
      } .elsewhen (io.memInstr.valid) {
        nextState := stateValid
      } .otherwise {
        nextState := stateFreeze
      }
    }

    is (stateValid) {
      printDbg("valid")
      when (io.branchPCIn.valid) {
        nextState := stateFlush
      } .elsewhen (!io.memInstr.valid | !io.instr.ready) {
        nextState := stateFreeze
      } 
    }

    is (stateFreeze) {
      printDbg("freeze")
      when (io.memInstr.valid) {
        when (io.instr.ready) {
          nextState := stateValid
        } .otherwise {
          nextState := stateFreezeOut
        }
      }
    }

    is (stateFreezeOut) {
      printDbg("freezeOut")
      when (io.instr.ready) {
        nextState := stateInitMem
      }
    }

    is (stateFlush) {
      printDbg("flush")
      when (io.memInstr.valid) {
        when (io.branchPCIn.valid) {
          nextState := stateInit
        } .otherwise {
          nextState := stateInitMem
        }
      }
    }
  }

  when (io.branchPCIn.valid) {
    pcReg := io.branchPCIn.bits.asUInt()
  } .elsewhen ((stateReg === stateInitMem) | (stateReg === stateInitMem2) | (stateReg === stateValid)) {
    pcReg := pcReg + (1.U << (pcAlign - 1))
  }

  when ((stateReg === stateInitMem) | (stateReg === stateInitMem2) | (stateReg === stateValid)) {
    pcBufReg := pcReg
  }

  when (io.memInstr.valid) {
    instrReg.word := io.memInstr.bits
    instrReg.pc := pcBufReg
  }

  io.pcOut.valid := (stateReg === stateInitMem) | (stateReg === stateInitMem2) | (stateReg === stateValid)
  io.pcOut.bits := pcReg
  io.memInstr.ready := true.B
  io.instr.valid := (stateReg === stateValid)
  io.instr.bits := instrReg

  def printDbg(state : String) = {
    if (dbgMsg) {
      printf("Fetch state = " + "%12s".format(state) + ", instr = { valid=%b, pc= %x, word=%x }\n", io.instr.valid, io.instr.bits.pc, io.instr.bits.word)
    }
  }
}
