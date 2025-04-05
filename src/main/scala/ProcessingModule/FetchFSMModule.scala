package ProcessingModule

import chisel3._
import chisel3.util.{switch, is}

class FetchFSMModule(iWidth : Int, pcWidth : Int, pcAlign : Int) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(SInt(pcWidth.W)))
    val pcOut = util.Valid(UInt(pcWidth.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(new Instruction(iWidth, pcWidth))
  })

  val stateInit :: stateInitMem :: stateReady :: stateWaitOut :: stateWaitMem :: stateWaitOutMem :: stateBranch :: stateBranchMem :: Nil = util.Enum(8)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printf("Fetch state = init\n")
      when (io.branchPCIn.valid) {
        nextState := stateBranch
      } .elsewhen (io.instr.ready) {
        nextState := stateInitMem
      }
    }

    is (stateBranch) {
      printf("Fetch state = branch\n")
      nextState := stateInit
    }

    is (stateBranchMem) {
      printf("Fetch state = branchMem\n")
      when (io.memInstr.valid) {
        nextState := stateInit
      }
    }

    is (stateInitMem) {
      printf("Fetch state = initMem\n")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (io.memInstr.valid & io.instr.ready) {
        nextState := stateReady
      } .elsewhen (!io.memInstr.valid & !io.instr.ready) {
        nextState := stateWaitOutMem
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateWaitMem
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      }
    }

    is (stateReady) {
      printf("Fetch state = ready\n")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (!io.memInstr.valid & !io.instr.ready) {
        nextState := stateWaitOutMem
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateWaitMem
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      }
    }

    is (stateWaitOut) {
      printf("Fetch state = waitOut\n")
      when (io.branchPCIn.valid) {
        nextState := stateBranch
      } .elsewhen (io.instr.ready) {
        nextState := stateReady
      }
    }

    is (stateWaitMem) {
      printf("Fetch state = waitMem\n")
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
          nextState := stateReady
        }
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOutMem
      }
    }

    is (stateWaitOutMem) {
      printf("Fetch state = waitOutMem\n")
      when (io.branchPCIn.valid) {
        when (io.memInstr.valid) {
          nextState := stateBranch
        } .otherwise {
          nextState := stateBranchMem
        }
      } .elsewhen (io.instr.ready & io.memInstr.valid) {
        nextState := stateReady
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
  when ((stateReg === stateInitMem) | (stateReg === stateReady)) {
    pcReg := pcReg + (1.U << (pcAlign - 1))
    instrReg.pc := pcReg
  } .elsewhen ((stateReg === stateBranch) | (stateReg === stateBranchMem)) {
    pcReg := branchPCReg
  }

  io.pcOut.valid := (stateReg === stateInitMem) | (stateReg === stateReady)
  io.pcOut.bits := pcReg

  when (io.memInstr.valid) {
    instrReg.word := io.memInstr.bits
  }
  io.memInstr.ready := true.B

  io.instr.bits := instrReg
  io.instr.valid := (stateReg === stateReady)
}
