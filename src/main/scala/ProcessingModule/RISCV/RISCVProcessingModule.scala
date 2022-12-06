package ProcessingModule.RISCV

import chisel3._
import ProcessingModule.InstructionLogic

class ADDI extends InstructionLogic("addi") {

  def getOpcode(instr : UInt) : UInt = instr(6,2)
  def isIType(instr : UInt) : Bool = getOpcode(instr) === "b00_100".U

  def getFunc(instr : UInt) : UInt = instr(14,12)

  def getIImm(instr : UInt) : UInt = instr(31,20)
  def getIDest(instr : UInt) : UInt = instr(11,7)
  def getISrc(instr : UInt) : UInt = instr(19,15)

  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isIType(instr) & (getFunc(instr) === "b000".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = getISrc(instr)
  override def writeRF() : Bool = true.B
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + getIImm(instr)
}

class RISCVInstructions extends ProcessingModule.Instructions {

  override def logic = new ProcessingModule.InstructionLogic("dummy"){
    override def decode(instr : UInt) : Bool = false.B
  } ::
  new ADDI ::
  Nil
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
