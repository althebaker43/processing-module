package ProcessingModule

import chisel3._
import chisel3.util.{switch, is}

class FetchFSMModule(iWidth : Int, pcWidth : Int, pcAlign : Int, val dbgMsg : Boolean = true) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(SInt(pcWidth.W)))
    val pcOut = util.Valid(UInt(pcWidth.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(new Instruction(iWidth, pcWidth))
  })

  val stateInit :: stateInitMem :: stateBufPC :: stateFlushPC :: stateReady :: stateWaitOut :: stateFlushInstr :: stateWaitMem :: stateWaitOutMem :: stateBranch :: stateBranchMem :: stateBranchFlush :: Nil = util.Enum(12)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val pcBufReg = Reg(util.Valid(UInt(pcWidth.W)))

  def printDbg(state : String) = {
    if (dbgMsg) {
      printf("Fetch state = " + "%12s".format(state) + ", instr = { valid=%b, pc= %x, word=%x }\n", io.instr.valid, io.instr.bits.pc, io.instr.bits.word)
    }
  }

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printDbg("init")
      when (io.branchPCIn.valid) {
        nextState := stateBranch
      } .elsewhen (io.instr.ready) {
        nextState := stateInitMem
      }
    }

    is (stateBranch) {
      printDbg("branch")
      nextState := stateBranchFlush
    }

    is (stateBranchMem) {
      printDbg("branchMem")
      when (io.memInstr.valid) {
        nextState := stateBranchFlush
      }
    }

    is (stateBranchFlush) {
      printDbg("branchFlush")
      when (io.branchPCIn.valid) {
        nextState := stateBranch
      } .elsewhen (io.instr.ready) {
        nextState := stateInitMem
      }
    }

    is (stateInitMem) {
      printDbg("initMem")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (io.memInstr.valid & io.instr.ready) {
        nextState := stateReady
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      } .otherwise {
        nextState := stateBufPC
      }
    }

    is (stateBufPC) {
      printDbg("bufPC")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (io.memInstr.valid & io.instr.ready) {
        nextState := stateReady
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateWaitMem
      } .otherwise {
        nextState := stateWaitOutMem
      }
    }

    is (stateFlushPC) {
      printDbg("flushPC")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      }.elsewhen (io.memInstr.valid & io.instr.ready) {
        nextState := stateReady
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateBufPC
      } .otherwise {
        nextState := stateWaitOutMem
      }
    }

    is (stateReady) {
      printDbg("ready")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (!io.memInstr.valid & !io.instr.ready) {
        nextState := stateBufPC
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateBufPC
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      }
    }

    is (stateWaitOut) {
      printDbg("waitOut")
      when (io.branchPCIn.valid) {
        nextState := stateBranch
      } .elsewhen (io.instr.ready) {
        when (pcBufReg.valid) {
          nextState := stateFlushPC
        } .otherwise {
          nextState := stateFlushInstr
        }
      }
    }

    is (stateFlushInstr) {
      printDbg("flushInstr")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (!io.memInstr.valid & !io.instr.ready) {
        nextState := stateBufPC
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateBufPC
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      }
    }

    is (stateWaitMem) {
      printDbg("waitMem")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (io.memInstr.valid) {
        when (!io.instr.ready) {
          nextState := stateWaitOut
        } .otherwise {
          nextState := stateFlushPC
        }
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOutMem
      }
    }

    is (stateWaitOutMem) {
      printDbg("waitOutMem")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (io.instr.ready & io.memInstr.valid) {
        when (pcBufReg.valid) {
          nextState := stateFlushPC
        } .otherwise {
          nextState := stateReady
        }
      } .elsewhen (io.instr.ready) {
        nextState := stateWaitMem
      } .elsewhen (io.memInstr.valid) {
        nextState := stateWaitOut
      }
    }
  }

  val branchPCReg = RegInit(0.U(pcWidth.W))
  when (io.branchPCIn.valid) {
    branchPCReg := io.branchPCIn.bits.asUInt()
  }

  val pcReg = RegInit(0.U(pcWidth.W))
  val instrReg = Reg(new Instruction(iWidth, pcWidth))
  val memInstrBufReg = RegInit(0.U(iWidth.W))

  val nextPC = Wire(UInt(pcWidth.W))
  nextPC := pcReg + (1.U << (pcAlign - 1))

  when ((stateReg === stateInitMem) | (stateReg === stateReady) | (stateReg === stateFlushPC) | (stateReg === stateFlushInstr) | (stateReg === stateBranchFlush)) {
    pcReg := nextPC
    pcBufReg.bits := pcReg
    instrReg.pc := pcBufReg.bits
  } .elsewhen ((stateReg === stateBufPC) & io.memInstr.valid & io.instr.ready) {
    pcReg := nextPC
    pcBufReg.bits := pcReg
    instrReg.pc := pcBufReg.bits
  } .elsewhen ((stateReg === stateBranch) | (stateReg === stateBranchMem)) {
    pcReg := branchPCReg
  } .elsewhen (stateReg === stateInit) {
    pcBufReg.valid := false.B
  }

  io.pcOut.valid := (stateReg === stateInitMem) | (stateReg === stateBufPC) | (stateReg === stateReady) | (stateReg === stateBranchFlush)
  io.pcOut.bits := pcReg

  when (io.memInstr.valid) {
    when (stateReg === stateWaitOut) {
      memInstrBufReg := io.memInstr.bits
    } .otherwise {
      instrReg.word := io.memInstr.bits
    }
  }
  when (stateReg === stateFlushInstr) {
    instrReg.word := memInstrBufReg
  }
  io.memInstr.ready := (stateReg =/= stateWaitOut)

  io.instr.bits := instrReg
  io.instr.valid := (stateReg === stateReady) | (stateReg === stateFlushPC) | (stateReg === stateFlushInstr)
}
