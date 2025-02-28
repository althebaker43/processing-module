package ProcessingModule.RISCV

import chisel3._
import ProcessingModule.InstructionLogic

abstract class RISCVInstructionLogic(name : String) extends InstructionLogic(name) {

  def getOpcode(instr : UInt) : UInt = instr(6,2)
  def isOpImm(instr : UInt) : Bool = getOpcode(instr) === "b00_100".U
  def isLUI(instr : UInt) : Bool = getOpcode(instr) === "b01_101".U
  def isAUIPC(instr : UInt) : Bool = getOpcode(instr) === "b00_101".U
  def isBranch(instr : UInt) : Bool = getOpcode(instr) === "b11_000".U
  def isSystem(instr : UInt) : Bool = getOpcode(instr) === "b11_100".U
  def isJAL(instr : UInt) : Bool = getOpcode(instr) === "b11_011".U
  def isJALR(instr : UInt) : Bool = getOpcode(instr) === "b11_001".U
  def isMiscMem(instr : UInt) : Bool = getOpcode(instr) === "b00_011".U

  def getFunc(instr : UInt) : UInt = instr(14,12)

  def getIImm(instr : UInt) : UInt = instr(31,20)
  def getIDest(instr : UInt) : UInt = instr(11,7)
  def getISrc(instr : UInt) : UInt = instr(19,15)

  def getUImm(instr : UInt) : UInt = instr(31,12)
  def getUDest(instr : UInt) : UInt = instr(11,7)

  def getBSrc1(instr : UInt) : UInt = instr(19,15)
  def getBSrc2(instr : UInt) : UInt = instr(24,20)
  def getBImm(instr : UInt) : UInt = util.Cat(instr(31), instr(7), instr(30,25), instr(11,8), 0.U(1.W))

  def getJDest(instr : UInt) : UInt = instr(11,7)
  def getJImm(instr : UInt) : UInt = util.Cat(instr(31), instr(19,12), instr(20), instr(30,21), 0.U(1.W))
}

class ADDI extends RISCVInstructionLogic("addi") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isOpImm(instr) & (getFunc(instr) === "b000".U) & (getIDest(instr) =/= 0.U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = getISrc(instr)
  override def writeRF(instr : UInt) : Bool = true.B
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + getIImm(instr)
}

class LUI extends RISCVInstructionLogic("lui") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isLUI(instr)
  override def getRFIndex(instr: UInt, opIndex: Int): UInt = getUDest(instr)
  override def writeRF(instr : UInt) : Bool = true.B
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getUDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt =  (getUImm(instr) << 12) | ops(0)(11, 0)
}

class AUIPC extends RISCVInstructionLogic("auipc") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isAUIPC(instr) & (getUDest(instr) =/= 0.U)
  override def writeRF(instr : UInt) : Bool = true.B
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getUDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = (getUImm(instr) << 12) | pc(11, 0)
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
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0) === ops(1), getBImm(instr).asSInt, 4.S)
}

class BNE extends RISCVInstructionLogic("bne") {
  override val numOps : Int = 2
  override def decode(instr : UInt) : Bool = isBranch(instr) & (getFunc(instr) === "b001".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
    opIndex match {
      case 0 => getBSrc1(instr)
      case 1 => getBSrc2(instr)
      case _ => 0.U
    }
  }
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0) =/= ops(1), getBImm(instr).asSInt, 4.S)
}

class BGE extends RISCVInstructionLogic("bge") {
  override val numOps : Int = 2
  override def decode(instr : UInt) : Bool = isBranch(instr) & (getFunc(instr) === "b101".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
    opIndex match {
      case 0 => getBSrc1(instr)
      case 1 => getBSrc2(instr)
      case _ => 0.U
    }
  }
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0).asSInt >= ops(1).asSInt, getBImm(instr).asSInt, 4.S)
}

class BGEU extends RISCVInstructionLogic("bgeu") {
  override val numOps : Int = 2
  override def decode(instr : UInt) : Bool = isBranch(instr) & (getFunc(instr) === "b111".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
    opIndex match {
      case 0 => getBSrc1(instr)
      case 1 => getBSrc2(instr)
      case _ => 0.U
    }
  }
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0) >= ops(1), getBImm(instr).asSInt, 4.S)
}

class BLT extends RISCVInstructionLogic("blt") {
  override val numOps : Int = 2
  override def decode(instr : UInt) : Bool = isBranch(instr) & (getFunc(instr) === "b100".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
    opIndex match {
      case 0 => getBSrc1(instr)
      case 1 => getBSrc2(instr)
      case _ => 0.U
    }
  }
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0).asSInt < ops(1).asSInt, getBImm(instr).asSInt, 4.S)
}

class BLTU extends RISCVInstructionLogic("bltu") {
  override val numOps : Int = 2
  override def decode(instr : UInt) : Bool = isBranch(instr) & (getFunc(instr) === "b110".U)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
    opIndex match {
      case 0 => getBSrc1(instr)
      case 1 => getBSrc2(instr)
      case _ => 0.U
    }
  }
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = Mux(ops(0) < ops(1), getBImm(instr).asSInt, 4.S)
}

class CSRRW extends RISCVInstructionLogic("csrrw") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b001".U)
  override def getRFIndex(instr: UInt, opIndex: Int): UInt = getISrc(instr)
  override def readMemory(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def writeMemory(instr : UInt) : Bool = true.B
  override def writeRF(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = getIImm(instr)
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0)
  override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData
}

class CSRRS extends RISCVInstructionLogic("csrrs") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b010".U)
  override def getRFIndex(instr: UInt, opIndex: Int): UInt = getISrc(instr)
  override def readMemory(instr : UInt) : Bool = true.B
  override def writeMemory(instr : UInt) : Bool = getISrc(instr) =/= 0.U 
  override def writeRF(instr : UInt) : Bool = true.B
  override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = getIImm(instr)
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0)
  override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData
  override def getMemWriteData(resultData : UInt, memData : UInt) : UInt = memData | resultData
}

class CSRRC extends RISCVInstructionLogic("csrrc") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b011".U)
  override def getRFIndex(instr: UInt, opIndex: Int): UInt = getISrc(instr)
  override def readMemory(instr : UInt) : Bool = true.B
  override def writeMemory(instr : UInt) : Bool = getISrc(instr) =/= 0.U 
  override def writeRF(instr : UInt) : Bool = true.B
  override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = getIImm(instr)
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0)
  override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData
  override def getMemWriteData(resultData : UInt, memData : UInt) : UInt = memData & ~resultData
}

class CSRRWI extends RISCVInstructionLogic("csrrwi") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b101".U)
  override def readMemory(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def writeMemory(instr : UInt) : Bool = true.B
  override def writeRF(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = getIImm(instr)
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = getISrc(instr)
  override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData
}

class CSRRSI extends RISCVInstructionLogic("csrrsi") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b110".U)
  override def readMemory(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def writeMemory(instr : UInt) : Bool = true.B
  override def writeRF(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = getIImm(instr)
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = getISrc(instr)
  override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData | resultData
}

class CSRRCI extends RISCVInstructionLogic("csrrci") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b111".U)
  override def readMemory(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def writeMemory(instr : UInt) : Bool = true.B
  override def writeRF(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = getIImm(instr)
  override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = getISrc(instr)
  override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData & ~resultData
}

class ECALL extends RISCVInstructionLogic("ecall") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isSystem(instr) & (getFunc(instr) === "b000".U)
  override def writeMemory(instr : UInt) : Bool = true.B
  override def getAddress(instr: UInt, ops: Vec[UInt]): UInt = 0.U
  override def getData(instr: UInt, pc: UInt, ops: Vec[UInt]): UInt = 1.U
}

class FENCE extends RISCVInstructionLogic("fence") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isMiscMem(instr) & (getFunc(instr) === "b000".U)
}

class JAL extends RISCVInstructionLogic("jal") {
  override val numOps : Int = 0
  override def decode(instr : UInt) : Bool = isJAL(instr)
  override def branch() : Bool = true.B
  override def relativeBranch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = util.Cat(getJImm(instr), 0.U(1.W)).asSInt
  override def writeRF(instr : UInt) : Bool = getJDest(instr) =/= 0.U
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = pc + 4.U
}

class JALR extends RISCVInstructionLogic("jalr") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isJALR(instr)
  override def getRFIndex(instr : UInt, opIndex : Int) : UInt = getISrc(instr)
  override def branch() : Bool = true.B
  override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = util.Cat(getIImm(instr).asSInt + util.Cat(0.U(1.W), ops(0)).asSInt, 0.U(1.W)).asSInt
  override def writeRF(instr : UInt) : Bool = getIDest(instr) =/= 0.U
  override def getWriteIndex(instr: UInt, ops: Vec[UInt]): UInt = getIDest(instr)
  override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = pc + 4.U
}

class SLLI extends RISCVInstructionLogic("slli") {
  override val numOps : Int = 1
  override def decode(instr : UInt) : Bool = isOpImm(instr) & (getFunc(instr) === "b001".U)
  override def getRFIndex(instr: UInt, opIndex: Int): UInt = getISrc(instr)
  override def writeRF(instr: UInt): Bool = true.B
  override def getWriteIndex(instr: UInt, ops: Vec[UInt]): UInt = getIDest(instr)
  override def getData(instr: UInt, pc: UInt, ops: Vec[UInt]): UInt = ops(1) << getIImm(instr)(4,0)
}

class RISCVInstructions extends ProcessingModule.Instructions {

  override def logic = new ProcessingModule.InstructionLogic("dummy"){
    override def decode(instr : UInt) : Bool = false.B
  } ::
  new ADDI ::
  new AUIPC ::
  new BEQ ::
  new BNE ::
  new BGE ::
  new BGEU ::
  new BLT ::
  new BLTU ::
  new CSRRW ::
  new CSRRS ::
  new CSRRC ::
  new CSRRWI ::
  new CSRRSI ::
  new CSRRCI ::
  new ECALL ::
  new FENCE ::
  new JAL ::
  new JALR ::
  new LUI ::
  new SLLI ::
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
