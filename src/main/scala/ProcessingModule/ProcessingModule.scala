
package ProcessingModule

import chisel3._

abstract class Instructions {

  def logic : Seq[InstructionLogic]
}

abstract class InstructionLogic(val name : String, val dataInDepend : Boolean, val dataOutDepend : Boolean) {

  val numOps : Int = 0

  def decode( instr : UInt) : Bool

  def getRFIndex(instr : UInt, opIndex : Int) : UInt = 0.U

  def branch() : Bool = false.B

  def relativeBranch() : Bool = false.B

  def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = 0.S

  def readMemory() : Bool = false.B

  def writeMemory() : Bool = false.B

  def writeRF() : Bool = false.B

  def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = 0.U

  def getWriteIndex(intr : UInt, ops : Vec[UInt]) : UInt = 0.U

  def getData(instr : UInt, ops : Vec[UInt]) : UInt = 0.U

  def getRFWriteData(resultData : UInt, memData : UInt) : UInt = memData

  def execute( instr : UInt) : Unit

  def load(instr : UInt) : UInt = 0.U

  def store(instr : UInt) : UInt = 0.U
}

abstract class ProcessingModule(dWidth : Int, dAddrWidth : Int, iWidth : Int, numOps : Int, opWidth : Int, rfDepth : Int) extends Module {

  val io = IO(new Bundle {
    val instr = new Bundle {
      val in = Flipped(util.Decoupled(UInt(iWidth.W)))
      val pc = util.Valid(UInt(64.W))
    }
    val data = new Bundle {
      val in = Flipped(util.Decoupled(UInt(dWidth.W)))
      val out = new Bundle {
        val addr = util.Valid(UInt(dAddrWidth.W))
        val value = util.Decoupled(UInt(dWidth.W))
      }
    }
  });

  def initInstrs : Instructions

  val instrs = initInstrs

  val fetch = Module(new FetchModule(iWidth))
  fetch.io.pcOut <> io.instr.pc
  fetch.io.memInstr <> io.instr.in

  val decode = Module(new DecodeModule(iWidth, instrs, numOps, opWidth, dWidth, rfDepth))
  decode.io.instrIn <> fetch.io.instr
  fetch.io.branchPCIn <> decode.io.branchPC
  fetch.io.relativeBranch := decode.io.relativeBranch

  val execute = Module(new ExecuteModule(iWidth, instrs, numOps, dWidth, dWidth, dAddrWidth, rfDepth))
  execute.io.instr <> decode.io.instrOut
  execute.io.instrValids <> decode.io.instrValids
  execute.io.ops <> decode.io.ops
  decode.io.exData.bits := execute.io.results.data
  decode.io.exData.valid := execute.io.results.writeRF & ~execute.io.results.readMem & ~execute.io.results.writeMem
  decode.io.exIndex := execute.io.results.rfIndex

  val memory = Module(new MemoryModule(dWidth, dAddrWidth, rfDepth, instrs))
  memory.io.results <> execute.io.results
  memory.io.memAddr <> io.data.out.addr
  memory.io.memDataOut <> io.data.out.value
  memory.io.memDataIn <> io.data.in
  decode.io.data <> memory.io.rfDataOut
  decode.io.index <> memory.io.rfIndexOut
}

object AdderInstruction {

  val codeNOP :: codeIncr1 :: codeIncrData :: codeStore :: codeBGT :: Nil = util.Enum(5)

  val codeWidth = codeNOP.getWidth

  val regWidth = 1

  val addrWidth = 4

  val width = codeWidth + regWidth + addrWidth

  def createInt(codeVal : UInt, regVal : UInt, addrVal : UInt = 0.U) : BigInt = (new AdderInstruction).createInt(codeVal, regVal, addrVal)
}

class AdderInstruction extends Bundle {

  val code : UInt = UInt(AdderInstruction.codeWidth.W)
  val reg : UInt = UInt(AdderInstruction.regWidth.W)
  val addr : UInt = UInt(AdderInstruction.addrWidth.W)

  def createInt(codeVal : UInt, regVal : UInt, addrVal : UInt) : BigInt = {
    def catValWidths(valWidthA : Tuple2[BigInt, Int], valWidthB : Tuple2[BigInt, Int]) : Tuple2[BigInt, Int] = {
      (((valWidthA._1 << valWidthB._2) | valWidthB._1), (valWidthA._2 + valWidthB._2))
    }
    val valWidths = ((addrVal :: regVal :: codeVal :: Nil) map (_.litValue)) zip (getElements map (_.getWidth))
    (valWidths reduce catValWidths)._1
  }
}

class AdderModule(dWidth : Int) extends ProcessingModule(dWidth, AdderInstruction.addrWidth, AdderInstruction.width, 2, dWidth, 2) {

  def getInstrCode(instr : UInt) : UInt = instr(2,0)
  def getInstrReg(instr : UInt) : UInt = instr(3)
  def getInstrAddr(instr : UInt) : UInt = instr(7,4)

  def initInstrs = new Instructions {

    def logic = {
      new InstructionLogic("nop", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeNOP
        def execute ( instr : UInt ) : Unit = Unit
      } ::
      new InstructionLogic("incr1", dataInDepend=false, dataOutDepend=false) {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeIncr1
        override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def writeRF : Bool = true.B
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) = getInstrReg(instr)
        override def getData(instr : UInt, ops : Vec[UInt]) = ops(0) + 1.U
        def execute ( instr : UInt ) : Unit = Unit
      } ::
      new InstructionLogic("incrData", dataInDepend=true, dataOutDepend=false) {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeIncrData
        override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def writeRF : Bool = true.B
        override def readMemory : Bool = true.B
        override def getAddress(instr : UInt, ops : Vec[UInt]) = getInstrAddr(instr)
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) = getInstrReg(instr)
        override def getRFWriteData(resultData : UInt, memData : UInt) : UInt = resultData + memData
        def execute ( instr : UInt ) : Unit = Unit
      } ::
      new InstructionLogic("store", dataInDepend=false, dataOutDepend=true) {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeStore
        override def getRFIndex(instr : UInt, opIndex : Int) = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def writeMemory : Bool = true.B
        override def getAddress(instr : UInt, ops : Vec[UInt]) = getInstrAddr(instr)
        override def getData(instr : UInt, ops : Vec[UInt]) = ops(0)
        def execute ( instr : UInt ) : Unit = Unit
      } ::
      new InstructionLogic("bgt", dataInDepend=false, dataOutDepend=false) {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeBGT
        override def getRFIndex(instr : UInt, opIndex : Int) = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def branch = true.B
        override def relativeBranch = true.B
        override def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = {
          val offset = Wire(SInt())
          when (ops(0) > 0.U) {
            offset := 2.S
          } .otherwise {
            offset := 1.S
          }
          offset
        }
        def execute ( instr : UInt ) : Unit = Unit
      } ::
      Nil
    }
  }
}

class FetchModule(iWidth : Int) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(SInt(64.W)))
    val relativeBranch = Input(Bool())
    val pcOut = util.Valid(UInt(64.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(UInt(iWidth.W))
  })

  val memReadyReg = RegInit(false.B)
  io.memInstr.ready := memReadyReg
  when (io.memInstr.valid & memReadyReg & ~io.instr.ready) {
    memReadyReg := false.B
  } .elsewhen (~io.memInstr.valid & ~memReadyReg & io.instr.ready) {
    memReadyReg := true.B
  }

  val instrReg = RegInit(0.U(iWidth.W))
  val instrValid = Wire(Bool())
  instrValid := io.memInstr.valid & memReadyReg & ~io.branchPCIn.valid
  when (instrValid) {
    instrReg := io.memInstr.bits
  }
  val instrValidReg = RegNext(instrValid)
  io.instr.bits := instrReg
  io.instr.valid := instrValidReg

  val pcReg = RegInit(0.U(64.W))
  when (io.memInstr.valid & memReadyReg) {
    when (~io.branchPCIn.valid) {
      pcReg := io.pcOut.bits + 1.U
    } .otherwise {
      pcReg := io.pcOut.bits
    }
  }
  val pcValidReg = RegNext(~io.memInstr.valid | memReadyReg)
  val branchValidReg = RegNext(io.branchPCIn.valid)
  io.pcOut.valid := pcValidReg
  when (io.branchPCIn.valid & (~branchValidReg)) {
    when (~io.relativeBranch) {
      io.pcOut.bits := io.branchPCIn.bits.asUInt
    } .otherwise {
      io.pcOut.bits := (pcReg.asSInt + io.branchPCIn.bits).asUInt
    }
  } .otherwise {
    io.pcOut.bits := pcReg
  }
}

class DecodeModule(
  iWidth : Int,
  instrs : Instructions,
  numOps : Int,
  opWidth : Int,
  rfWidth : Int,
  rfDepth : Int
) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle {
    val instrIn = Flipped(util.Decoupled(UInt(iWidth.W)))
    val data = Flipped(util.Valid(UInt(rfWidth.W)))
    val index = Input(UInt(rfIdxWidth.W))
    val exData = Flipped(util.Valid(UInt(rfWidth.W)))
    val exIndex = Input(UInt(rfIdxWidth.W))
    val instrValids = Output(Vec(numInstrs, Bool()))
    val ops = Output(Vec(numOps, UInt(opWidth.W)))
    val branchPC = util.Valid(SInt(64.W))
    val relativeBranch = Output(Bool())
    val instrOut = Output(UInt(iWidth.W))
  })

  val instrValidsReg = Reg(Vec(numInstrs, Bool()))
  val instrValidsRegIn = Wire(Vec(numInstrs, Bool()))
  val hazard = Wire(Bool())
  val hazardReg = RegInit(false.B)
  for (idx <- 0 until numInstrs) {
    instrValidsReg(idx) := instrValidsRegIn(idx) & !hazard
    io.instrValids(idx) := instrValidsReg(idx)
  }

  val rf = RegInit(VecInit(Seq.fill(rfDepth){ 0.U(rfWidth.W) }))
  val rfWritten = RegInit(VecInit(Seq.fill(rfDepth){ true.B }))

  when (io.data.valid) {
    rf(io.index) := io.data.bits
    rfWritten(io.index) := true.B
  } .elsewhen (io.exData.valid) {
    rf(io.exIndex) := io.exData.bits
    rfWritten(io.exIndex) := true.B
  }

  val ops = Wire(Vec(numOps, UInt(opWidth.W)))
  for (opIdx <- 0 until numOps) {
    ops(opIdx) := 0.U(opWidth.W)
  }
  val opsReg = RegNext(ops)
  io.ops := opsReg

  io.branchPC.valid := false.B
  io.branchPC.bits := 0.S
  io.relativeBranch := false.B

  val instrReg = Reg(util.Valid(UInt(iWidth.W)))
  instrReg.bits := io.instrIn.bits
  instrReg.valid := io.instrIn.valid
  io.instrOut := instrReg.bits

  io.instrIn.ready := true.B
  hazard := false.B
  hazardReg := hazard

  val instrIn = Wire(util.Valid(UInt(iWidth.W)))
  when (!hazardReg) {
    instrIn.bits := io.instrIn.bits
    instrIn.valid := io.instrIn.valid
  } .otherwise {
    instrIn := instrReg
  }

  for ((instr, idx) <- instrs.logic.zipWithIndex) {

    when (instrIn.valid) {
      instrValidsRegIn(idx) := instr.decode(instrIn.bits)
      when (instrValidsRegIn(idx)) {
        for (opIdx <- 0 until instr.numOps) {
          val rfIdx = Wire(UInt(rfIdxWidth.W))
          rfIdx := instr.getRFIndex(instrIn.bits, opIdx)
          when (io.data.valid & (rfIdx === io.index)) {
            ops(opIdx) := io.data.bits
          } .elsewhen(io.exData.valid & (rfIdx === io.exIndex)) {
            ops(opIdx) := io.exData.bits
          } .otherwise {
            when (rfWritten(rfIdx)) {
              ops(opIdx) := rf(rfIdx)
            } .otherwise {
              io.instrIn.ready := false.B
              hazard := true.B
            }
          }
        }
        when (instr.writeRF) {
          rfWritten(instr.getWriteIndex(instrIn.bits, ops)) := false.B
        }
      }
    } .otherwise {
      instrValidsRegIn(idx) := false.B
    }

    when (instrValidsReg(idx)) {

      io.branchPC.valid := instr.branch
      io.branchPC.bits := instr.getBranchPC(instrReg.bits, opsReg)
      io.relativeBranch := instr.relativeBranch
    }
  }
}

class ExecuteResults(dataWidth : Int, addrWidth : Int, rfDepth : Int, numInstrs : Int) extends Bundle {

  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val addr = UInt(addrWidth.W)
  val rfIndex = UInt(rfIdxWidth.W)
  val data = UInt(dataWidth.W)
  val readMem = Bool()
  val writeMem = Bool()
  val writeRF = Bool()
  val instrValids = Vec(numInstrs, Bool())
  override def cloneType = (new ExecuteResults(dataWidth, addrWidth, rfDepth, numInstrs)).asInstanceOf[this.type]
}

class ExecuteModule(
  iWidth : Int,
  instrs : Instructions,
  numOps : Int,
  opWidth : Int,
  dataWidth : Int,
  addrWidth : Int,
  rfDepth : Int
) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle {
    val instr = Input(UInt(iWidth.W))
    val instrValids = Input(Vec(numInstrs, Bool()))
    val ops = Input(Vec(numOps, UInt(opWidth.W)))
    val results = Output(new ExecuteResults(dataWidth, addrWidth, rfDepth, numInstrs))
  })

  val results = Wire(new ExecuteResults(dataWidth, addrWidth, rfDepth, numInstrs))
  results.readMem := false.B
  results.writeMem := false.B
  results.writeRF := false.B
  results.addr := 0.U
  results.rfIndex := 0.U
  results.data := 0.U
  for ((instr, idx) <- instrs.logic.zipWithIndex) {
    when (io.instrValids(idx)) {
      results.readMem := instr.readMemory
      results.writeMem := instr.writeMemory
      results.writeRF := instr.writeRF
      when (instr.readMemory | instr.writeMemory | instr.writeRF) {
        results.addr := instr.getAddress(io.instr, io.ops)
        results.rfIndex := instr.getWriteIndex(io.instr, io.ops)
        results.data := instr.getData(io.instr, io.ops)
      }
    }
    results.instrValids(idx) := io.instrValids(idx)
  }

  val resultsReg = RegNext(results)
  io.results := resultsReg
}

class MemoryModule(dataWidth : Int, addrWidth : Int, rfDepth : Int, instrs : Instructions) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle{
    // TODO: decouple results in case of memory latency
    val results = Input(new ExecuteResults(dataWidth, addrWidth, rfDepth, numInstrs))
    val memAddr = util.Valid(UInt(addrWidth.W))
    val memDataOut = util.Decoupled(UInt(dataWidth.W))
    val memDataIn = Flipped(util.Decoupled(UInt(dataWidth.W)))
    val rfDataOut = util.Valid(UInt(dataWidth.W))
    val rfIndexOut = Output(UInt(rfIdxWidth.W))
  })

  val writeMemReg = RegInit(false.B)
  val readMemReg = RegInit(false.B)
  val writeRFReg = RegInit(false.B)
  val dataReg = RegInit(0.U(dataWidth.W))
  val addrReg = RegInit(0.U(addrWidth.W))
  val rfIdxInReg = RegInit(0.U(rfIdxWidth.W))
  val instrValidsReg = RegInit(VecInit(Seq.fill(numInstrs){ false.B }))

  val writeMem = Wire(Bool())
  val readMem = Wire(Bool())
  val writeRF = Wire(Bool())
  val data = Wire(UInt(dataWidth.W))
  val addr = Wire(UInt(addrWidth.W))
  val rfIdxIn = Wire(UInt(rfIdxWidth.W))
  val instrValids = Wire(Vec(numInstrs, Bool()))

  io.memAddr.valid := writeMem | readMem
  io.memAddr.bits := addr
  io.memDataOut.valid := writeMem
  io.memDataOut.bits := data
  io.memDataIn.ready := readMem

  val rfValidReg = RegInit(false.B)
  val rfDataReg = RegInit(0.U(dataWidth.W))
  val rfIdxOutReg = RegInit(0.U(rfIdxWidth.W))

  io.rfDataOut.valid := rfValidReg
  io.rfDataOut.bits := rfDataReg
  io.rfIndexOut := rfIdxOutReg

  val memBusy = Wire(Bool())
  val memBusyReg = RegInit(false.B)
  when (io.memAddr.valid) {
    when (io.memDataOut.valid) {
      memBusy := !io.memDataOut.ready
    } .otherwise {
      memBusy := !io.memDataIn.valid
    }
  } .otherwise {
    memBusy := false.B
  }
  memBusyReg := memBusy

  when (memBusy) {
    writeMemReg := writeMem
    readMemReg := readMem
    writeRFReg := writeRF
    dataReg := data
    addrReg := addr
    rfIdxInReg := rfIdxIn
    instrValidsReg := instrValids
  }

  when (!memBusyReg) {
    writeMem := io.results.writeMem
    readMem := io.results.readMem
    writeRF := io.results.writeRF
    data := io.results.data
    addr := io.results.addr
    rfIdxIn := io.results.rfIndex
    instrValids := io.results.instrValids
  } .otherwise {
    writeMem := writeMemReg
    readMem := readMemReg
    writeRF := writeRFReg
    data := dataReg
    addr := addrReg
    rfIdxIn := rfIdxInReg
    instrValids := instrValidsReg
  }

  when (writeRF) {
    when (readMem) {
      rfValidReg := io.memDataIn.valid
      for ((instr, idx) <- instrs.logic.zipWithIndex) {
        when (instrValids(idx)) {
          rfDataReg := instr.getRFWriteData(data, io.memDataIn.bits)
        }
      }
    } .otherwise {
      rfValidReg := true.B
      rfDataReg := data
    }
    rfIdxOutReg := rfIdxIn
  } .otherwise {
    rfValidReg := false.B
  }
}

class QueueModule extends Module {
  val io = IO(new Bundle{
    val in = Flipped(util.Decoupled(UInt(4.W)))
    val out = util.Decoupled(UInt(4.W))
  })

  val queueOut = util.Queue(io.in, 5)
  io.out.bits := queueOut.bits
  io.out.valid := queueOut.valid
  queueOut.ready := io.out.ready
}
