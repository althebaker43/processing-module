package ProcessingModule.RISCV

import chisel3._

class RISCVInstructions extends ProcessingModule.Instructions {

  override def logic = new ProcessingModule.InstructionLogic("dummy"){
    override def decode(instr : UInt) : Bool = false.B
  } :: Nil
}

class RISCVProcessingModule (rfDepth : Int)
    extends ProcessingModule.ProcessingModule(dWidth = 32,
      dAddrWidth = 32,
      iWidth = 32,
      pcWidth = 32,
      pcAlign = 3,
      numOps = 2,
      opWidth = 32,
      rfDepth = rfDepth) {

  override def initInstrs = new RISCVInstructions
}
