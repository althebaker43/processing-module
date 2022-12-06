package ProcessingModule.RISCV

import chisel3._
import ProcessingModule.InstructionLogic

abstract class RISCVInstructionLogic(name : String) extends InstructionLogic(name) {

  def getOpcode(instr : UInt) : UInt = instr(6,2)
  def isOpImm(instr : UInt) : Bool = getOpcode(instr) === "b00_100".U
  def isAUIPC(instr : UInt) : Bool = getOpcode(instr) === "b00_101".U
  def isBranch(instr : UInt) : Bool = getOpcode(instr) === "b11_000".U

  def getFunc(instr : UInt) : UInt = instr(14,12)

  def getIImm(instr : UInt) : UInt = instr(31,20)
  def getIDest(instr : UInt) : UInt = instr(11,7)
  def getISrc(instr : UInt) : UInt = instr(19,15)

  def getUImm(instr : UInt) : UInt = instr(31,12)
  def getUDest(instr : UInt) : UInt = instr(11,7)

  def getBSrc1(instr : UInt) : UInt = instr(19,15)
  def getBSrc2(instr : UInt) : UInt = instr(24,20)
  def getBImm(instr : UInt) : UInt = util.Cat(instr(31), instr(7), instr(30,25), instr(11,8), 0.U(1.W))
}

class ADDI extends RISCVInstructionLogic("addi") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isOpImm(instr) & (getFunc(instr) === "b000".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = getISrc(instr)
  override def writeRF() : Bool = true.B
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + getIImm(instr)
}

class AUIPC extends RISCVInstructionLogic("auipc") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isAUIPC(instr)
  override def writeRF() : Bool = true.B
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getUDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = getUImm(instr)
}

class BEQ extends RISCVInstructionLogic("beq") {
  override val numOps : Int = 2
  override def decode(instr : UInt) : Bool = isBranch(instr) & (getFunc(instr) === "b000".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
    opIndex match {
      case 0 => getBSrc1(instr)
      case 1 => getBSrc2(instr)
      case _ => 0.U
    }
  }
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0) === ops(1), getBImm(instr).asSInt, 0.S)
}

class RISCVInstructions extends ProcessingModule.Instructions {

  override def logic = new ProcessingModule.InstructionLogic("dummy"){
    override def decode(instr : UInt) : Bool = false.B
  } ::
  new ADDI ::
  new AUIPC ::
  new BEQ ::
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
