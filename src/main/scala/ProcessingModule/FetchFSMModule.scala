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

  val stateInit :: stateInitMem :: stateReady :: stateWaitOut :: stateWaitMem :: stateWaitOutMem :: Nil = util.Enum(6)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printf("Fetch state = init\n")
      when (io.instr.ready) {
        nextState := stateInitMem
      }
    }

    is (stateInitMem) {
      printf("Fetch state = initMem\n")
      when (io.memInstr.valid & io.instr.ready) {
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
      when (!io.memInstr.valid & !io.instr.ready) {
        nextState := stateWaitOutMem
      } .elsewhen (!io.memInstr.valid) {
        nextState := stateWaitMem
      } .elsewhen (!io.instr.ready) {
        nextState := stateWaitOut
      }
    }

    is (stateWaitOut) {
      printf("Fetch state = waitOut\n")
      when (io.instr.ready) {
        nextState := stateReady
      }
    }

    is (stateWaitMem) {
      printf("Fetch state = waitMem\n")
      when (io.memInstr.valid) {
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
      when (io.instr.ready & io.memInstr.valid) {
        nextState := stateReady
      } .elsewhen (io.instr.ready) {
        nextState := stateWaitMem
      } .elsewhen (io.memInstr.valid) {
        nextState := stateWaitOut
      }
    }
  }

  val pcReg = RegInit(0.U(pcWidth.W))
  val instrReg = Reg(new Instruction(iWidth, pcWidth))
  when ((stateReg === stateInitMem) | (stateReg === stateReady)) {
    pcReg := pcReg + (1.U << (pcAlign - 1))
    instrReg.pc := pcReg
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
