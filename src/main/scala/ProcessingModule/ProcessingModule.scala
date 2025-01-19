
package ProcessingModule

import chisel3._
import chisel3.util.{switch, is}

abstract class Instructions {

  def logic : Seq[InstructionLogic]
}

abstract class InstructionLogic(val name : String) {

  /** Number of operands */
  val numOps : Int = 0

  /** Indicates if the given word matches this instruction class */
  def decode(instr : UInt) : Bool

  /** Index in register file that specified operand is at */
  def getRFIndex(instr : UInt, opIndex : Int) : UInt = 0.U

  /** Indicates if this instruction is a branch */
  def branch() : Bool = false.B

  /** Indicates if this branch instruction is relative to PC */
  def relativeBranch() : Bool = false.B

  /** Returns instruction address or offset that should be jumped to */
  def getBranchPC(instr : UInt, ops : Vec[UInt]) : SInt = 0.S

  /** Indicates if this instruction reads from data memory */
  def readMemory(instr : UInt) : Bool = false.B

  /** Indicates if this instruction writes to data memory */
  def writeMemory(instr : UInt) : Bool = false.B

  /** Indicates if this instruction writes to the register file */
  def writeRF(instr : UInt) : Bool = false.B

  /** Returns data address that should be accessed */
  def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = 0.U

  /** Returns index in register file that should be written */
  def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = 0.U

  /** Returns data that should be written or stored */
  def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = 0.U

  /** Returns data that should be written to register file
    *
    * The data returned by this logic is calculated in the memory
    * stage using data just loaded from data memory
    */
  def getRFWriteData(resultData : UInt, memData : UInt) : UInt = memData

  /** Returns data that should be written to memory
    *
    * The data returned by this logic is calculated in the memory
    * stage using data just loaded from data memory
    */
  def getMemWriteData(resultData : UInt, memData : UInt) : UInt = resultData
}

abstract class ProcessingModule(
  dWidth : Int,
  dAddrWidth : Int,
  iWidth : Int,
  pcWidth : Int,
  pcAlign : Int,
  numOps : Int,
  opWidth : Int,
  rfDepth : Int) extends Module {

  val io = IO(new Bundle {
    val instr = new Bundle {
      val in = Flipped(util.Decoupled(UInt(iWidth.W)))
      val pc = util.Valid(UInt(pcWidth.W))
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

  val fetch = Module(new FetchModule(iWidth, pcWidth, pcAlign))
  fetch.io.pcOut <> io.instr.pc
  fetch.io.memInstr <> io.instr.in

  val decode = Module(new DecodeModule(iWidth, pcWidth, instrs, numOps, opWidth, dWidth, rfDepth))
  decode.io.instrIn <> fetch.io.instr
  fetch.io.branchPCIn <> decode.io.branchPC
  fetch.io.relativeBranch := decode.io.relativeBranch

  val execute = Module(new ExecuteModule(iWidth, pcWidth, instrs, numOps, dWidth, dWidth, dAddrWidth, rfDepth))
  execute.io.instr <> decode.io.instrOut
  execute.io.instrValids <> decode.io.instrValids
  decode.io.instrReady := execute.io.instrReady
  execute.io.ops <> decode.io.ops
  decode.io.exData.bits := execute.io.results.data
  decode.io.exData.valid := execute.io.results.writeRF & ~execute.io.results.readMem & ~execute.io.results.writeMem
  decode.io.exIndex := execute.io.results.rfIndex

  val memory = Module(new MemoryModule(dWidth, dAddrWidth, rfDepth, instrs))
  memory.io.results <> execute.io.results
  execute.io.resultsReady := memory.io.resultsReady
  memory.io.memAddr <> io.data.out.addr
  memory.io.memDataOut <> io.data.out.value
  memory.io.memDataIn <> io.data.in
  decode.io.data <> memory.io.rfDataOut
  decode.io.index <> memory.io.rfIndexOut
}

object AdderInstruction {

  val codeNOP :: codeIncr1 :: codeIncrData :: codeStore :: codeBGT :: Nil = util.Enum(5)

  val codeWidth = codeNOP.getWidth

  val regWidth = 2

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

class AdderModule(dWidth : Int)
    extends ProcessingModule(dWidth=dWidth,
      dAddrWidth=AdderInstruction.addrWidth,
      iWidth=AdderInstruction.width,
      pcWidth=6,
      pcAlign=1,
      numOps=2,
      opWidth=dWidth,
      rfDepth=4) {

  def getInstrCode(instr : UInt) : UInt = instr(2,0)
  def getInstrReg(instr : UInt) : UInt = instr(4, 3)
  def getInstrAddr(instr : UInt) : UInt = instr(8, 5)

  def initInstrs = new Instructions {

    def logic = {
      new InstructionLogic("nop") {
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeNOP
      } ::
      new InstructionLogic("incr1") {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeIncr1
        override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def writeRF(instr : UInt) : Bool = true.B
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) = getInstrReg(instr)
        override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) = ops(0) + 1.U
      } ::
      new InstructionLogic("incrData") {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeIncrData
        override def getRFIndex(instr : UInt, opIndex : Int) : UInt = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def writeRF(instr : UInt) : Bool = true.B
        override def readMemory(instr : UInt) : Bool = true.B
        override def getAddress(instr : UInt, ops : Vec[UInt]) = getInstrAddr(instr)
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) = getInstrReg(instr)
        override def getRFWriteData(resultData : UInt, memData : UInt) : UInt = resultData + memData
      } ::
      new InstructionLogic("store") {
        override val numOps : Int = 1
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeStore
        override def getRFIndex(instr : UInt, opIndex : Int) = {
          opIndex match {
            case 0 => getInstrReg(instr)
            case _ => 0.U
          }
        }
        override def writeMemory(instr : UInt) : Bool = true.B
        override def getAddress(instr : UInt, ops : Vec[UInt]) = getInstrAddr(instr)
        override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) = ops(0)
      } ::
      new InstructionLogic("bgt") {
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
            offset := 1.S
          } .otherwise {
            offset := 0.S
          }
          offset
        }
      } ::
      Nil
    }
  }
}

class Instruction(iWidth : Int, pcWidth : Int) extends Bundle {

  val word = UInt(iWidth.W)
  val pc = UInt(pcWidth.W)
  override def cloneType = (new Instruction(iWidth, pcWidth)).asInstanceOf[this.type]
}

class PCGenerator(pcWidth : Int, pcAlign : Int) extends Module {

  val io = IO(new Bundle {
    val pc = util.Decoupled(UInt(pcWidth.W))
  })

  val stateInit :: statePCOut :: stateWait :: Nil = util.Enum(3)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val pc = RegInit(0.S(pcWidth.W))

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      when (io.pc.ready) {
        nextState := statePCOut
      }
    }

    is (statePCOut) {
      when (!io.pc.ready) {
        nextState := stateWait
      }
    }

    is (stateWait) {
      when (io.pc.ready) {
        nextState := statePCOut
      }
    }
  }

  when (stateReg === stateInit) {
    pc := 0.S
  } .elsewhen (nextState === statePCOut) {
    pc := pc + (1.S << (pcAlign - 1))
  }

  io.pc.valid := (stateReg === statePCOut)
  io.pc.bits := pc.asUInt
}

class PCBuffer(iWidth : Int, pcWidth : Int, pcAlign : Int) extends Module {

  val io = IO(new Bundle {
    val pc = Flipped(util.Decoupled(UInt(pcWidth.W)))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(new Instruction(iWidth, pcWidth))
  })

  val stateInit :: stateReady :: stateBufPC :: stateInstrOut :: stateInstrOutBufPC :: stateWait :: stateWaitPC :: Nil = util.Enum(7)
  val stateReg = RegInit(stateInit)
  val nextState = Wire(UInt())
  stateReg := nextState

  val pcQueueIn = Wire(util.Decoupled(UInt(pcWidth.W)))
  val pcQueue = util.Queue(pcQueueIn, 2, pipe=true)

  nextState := stateReg
  switch (stateReg) {

    is (stateInit) {
      printf("PCBuffer state = init\n")
      when (io.instr.ready) {
        nextState := stateReady
      }
    }

    is (stateReady) {
      printf("PCBuffer state = ready\n")
      when (io.pc.valid) {
        nextState := stateBufPC
      }
    }

    is (stateBufPC) {
      printf("PCBuffer state = buf\n")
      when (io.memInstr.valid & io.instr.ready) {
        when (pcQueueIn.ready) {
          nextState := stateInstrOutBufPC
        } .otherwise {
          nextState := stateInstrOut
        }
      } .otherwise {
        nextState := stateWait
      }
      // when (io.memInstr.valid) {
      //   when (io.pc.valid) {
      //     nextState := stateInstrOutBufPC
      //   } .otherwise {
      //     nextState := stateInstrOut
      //   }
      // } .otherwise {
      //   nextState := stateWait
      // }
    }

    is (stateInstrOut) {
      printf("PCBuffer state = out\n")
      when (!io.memInstr.valid) {
        nextState := stateWait
      } .elsewhen (pcQueueIn.ready) {
        nextState := stateInstrOutBufPC
      }
      // nextState := stateWait
    }

    is (stateInstrOutBufPC) {
      printf("PCBuffer state = outBuf\n")
      when (io.memInstr.valid) {
        when (!pcQueueIn.ready) {
          nextState := stateInstrOut
        }
      } .otherwise {
        when (io.pc.valid) {
          nextState := stateWait
        } .otherwise {
          nextState := stateWaitPC
        }
      }
      // when (!io.memInstr.valid | !io.pc.valid) {
      //   nextState := stateWait
      // }
    }

    is (stateWait) {
      printf("PCBuffer state = wait\n")
      when (io.memInstr.valid & io.instr.ready) {
        when (pcQueueIn.ready) {
          nextState := stateInstrOutBufPC
        } .otherwise {
          nextState := stateInstrOut
        }
      }
      // when (io.pc.valid) {
      //   nextState := stateBufPC
      // } .elsewhen (io.memInstr.valid) {
      //   nextState := stateInstrOutBufPC
      // }
    }

    is (stateWaitPC) {
      printf("PCBuffer state = waitPC\n")
      when (io.pc.valid) {
        nextState := stateBufPC
      }
    }
  }

  val memInstrReg = RegNext(io.memInstr.bits)

  val pcReg = Reg(util.Valid(UInt(pcWidth.W)))
  when (io.pc.valid) {
    when (stateReg === stateBufPC) {
      pcReg.valid := false.B
    } .otherwise {
      pcReg.valid := io.pc.valid
      pcReg.bits := io.pc.bits
    }
  }

  pcQueueIn.valid := io.pc.valid
  pcQueueIn.bits := io.pc.bits
  when ((stateReg === stateInstrOut) | (stateReg === stateWait)) {
    pcQueueIn.valid := pcReg.valid
    pcQueueIn.bits := pcReg.bits
  }
  pcQueue.ready := (stateReg === stateInstrOut) | (stateReg === stateInstrOutBufPC)

  io.pc.ready := (stateReg =/= stateInit) & (stateReg =/= stateWait)
  io.memInstr.ready := stateReg =/= stateInit
  io.instr.valid := (stateReg === stateInstrOut) | (stateReg === stateInstrOutBufPC)
  io.instr.bits.pc := pcQueue.bits
  io.instr.bits.word := memInstrReg

  // // Ready output
  // io.memInstr.ready := io.instr.ready

  // // PC input
  // pcQueueIn.bits := io.pc.bits
  // pcQueueIn.valid := io.pc.valid
  // io.pc.ready := pcQueueIn.ready & (stateReg =/= stateBufPC)

  // // Instruction output
  // io.instr.valid := false.B
  // io.instr.bits.word := 0.U
  // io.instr.bits.pc := 0.U
  // pcQueue.ready := false.B
  // when (io.memInstr.valid) {
  //   when ((nextState === stateInstrOut) ||
  //     (nextState === stateInstrOutBufPC) ||
  //     (stateReg === stateInstrOut) ||
  //     (stateReg === stateInstrOutBufPC)) {
  //     io.instr.valid := pcQueue.valid
  //     io.instr.bits.pc := pcQueue.bits
  //     io.instr.bits.word := io.memInstr.bits
  //     when ((stateReg === stateInstrOut) ||
  //       (stateReg === stateInstrOutBufPC)) {
  //       pcQueue.ready := io.instr.ready
  //     }
  //   }
  // }
}

class FetchModule(iWidth : Int, pcWidth : Int, pcAlign : Int) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(SInt(pcWidth.W)))
    val relativeBranch = Input(Bool())
    val pcOut = util.Valid(UInt(pcWidth.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(new Instruction(iWidth, pcWidth))
  })

  val branchPC = Wire(UInt(pcWidth.W))
  val pcReg = RegInit(0.U(pcWidth.W))
  val nextPC = Wire(UInt(pcWidth.W))
  nextPC := pcReg + (1 << (pcAlign - 1)).U
  branchPC := Mux(io.relativeBranch, (pcReg.asSInt + io.branchPCIn.bits).asUInt, io.branchPCIn.bits.asUInt)

  val pcGen = Module(new PCGenerator(pcWidth, pcAlign))
  val pcBuf = Module(new PCBuffer(iWidth, pcWidth, pcAlign))
  pcBuf.io.pc <> pcGen.io.pc
  pcBuf.io.memInstr <> io.memInstr
  io.instr <> pcBuf.io.instr
  io.pcOut := pcGen.io.pc
}

class RFWord(rfWidth : Int) extends Bundle {

  val word = UInt(rfWidth.W)
  val isWritten = Bool()
  override def cloneType = (new RFWord(rfWidth)).asInstanceOf[this.type]
}

class DecodeModule(
  iWidth : Int,
  pcWidth : Int,
  instrs : Instructions,
  numOps : Int,
  opWidth : Int,
  rfWidth : Int,
  rfDepth : Int
) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle {
    val instrIn = Flipped(util.Decoupled(new Instruction(iWidth, pcWidth)))
    val data = Flipped(util.Valid(UInt(rfWidth.W)))
    val index = Input(UInt(rfIdxWidth.W))
    val exData = Flipped(util.Valid(UInt(rfWidth.W)))
    val exIndex = Input(UInt(rfIdxWidth.W))
    val instrValids = Output(Vec(numInstrs, Bool()))
    val ops = Output(Vec(numOps, UInt(opWidth.W)))
    val branchPC = util.Valid(SInt(pcWidth.W))
    val relativeBranch = Output(Bool())
    val instrOut = Output(new Instruction(iWidth, pcWidth))
    val instrReady = Input(Bool())
  })

  val instrValidsReg = Reg(Vec(numInstrs, Bool()))
  val instrValidsRegIn = Wire(Vec(numInstrs, Bool()))
  val instrValid = Wire(Bool())
  val hazard = Wire(Bool()) // TODO: rename and use also for memory latency in downstream stages
  val hazardReg = RegInit(false.B)
  for (idx <- 0 until numInstrs) {
    instrValidsReg(idx) := instrValidsRegIn(idx) & !hazard
    io.instrValids(idx) := instrValidsReg(idx)
  }

  val rf = Mem(rfDepth, new RFWord(rfWidth))

  val rfDataIn = Wire(new RFWord(rfWidth))
  rfDataIn.word := 0.U
  rfDataIn.isWritten := true.B
  val rfInitCounter = RegInit(0.U((rfIdxWidth+1).W))

  when (rfInitCounter =/= rfDepth.U) {
    rfDataIn.word := 0.U
    rfDataIn.isWritten := true.B
    rf.write(rfInitCounter, rfDataIn)
    rfInitCounter := rfInitCounter + 1.U
  } .elsewhen (io.data.valid) {
    rfDataIn.word := io.data.bits
    rfDataIn.isWritten := true.B
    rf.write(io.index, rfDataIn)
  } .elsewhen (io.exData.valid) {
    rfDataIn.word := io.exData.bits
    rfDataIn.isWritten := true.B
    rf.write(io.exIndex, rfDataIn)
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

  val instrReg = Reg(util.Valid(new Instruction(iWidth, pcWidth)))
  io.instrOut := instrReg.bits

  io.instrIn.ready := true.B
  when (rfInitCounter =/= rfDepth.U) {
    hazard := true.B
  } .otherwise {
    hazard := !io.instrReady
  }
  hazardReg := hazard

  val instrIn = Wire(util.Valid(new Instruction(iWidth, pcWidth)))
  when (!hazardReg) {
    instrIn.bits := io.instrIn.bits
    instrIn.valid := io.instrIn.valid
    instrReg.bits := io.instrIn.bits
    instrReg.valid := io.instrIn.valid
  } .otherwise {
    io.instrIn.ready := false.B
    instrIn := instrReg
  }

  val rfInstrAddr = Wire(UInt(rfIdxWidth.W))
  rfInstrAddr := 0.U
  val rfInstrIn = Wire(util.Valid(new RFWord(rfWidth)))
  rfInstrIn.bits.word := 0.U
  rfInstrIn.bits.isWritten := false.B
  rfInstrIn.valid := false.B
  when (rfInstrIn.valid) {
    rf.write(rfInstrAddr, rfInstrIn.bits)
  }

  instrValid := false.B
  for ((instr, idx) <- instrs.logic.zipWithIndex) {

    when (instrIn.valid) {
      instrValidsRegIn(idx) := instr.decode(instrIn.bits.word)
      when (instrValidsRegIn(idx)) {
        instrValid := true.B
        for (opIdx <- 0 until instr.numOps) {
          val rfIdx = Wire(UInt(rfIdxWidth.W))
          rfIdx := instr.getRFIndex(instrIn.bits.word, opIdx)
          when (io.data.valid & (rfIdx === io.index)) {
            ops(opIdx) := io.data.bits
          } .elsewhen(io.exData.valid & (rfIdx === io.exIndex)) {
            ops(opIdx) := io.exData.bits
          } .otherwise {
            when (rf(rfIdx).isWritten) {
              ops(opIdx) := rf(rfIdx).word
            } .otherwise {
              hazard := true.B
            }
          }
        }
        when (instr.writeRF(instrIn.bits.word) && !hazard) {
          rfInstrAddr := instr.getWriteIndex(instrIn.bits.word, ops)
          rfInstrIn.bits.word := rf(rfInstrAddr).word
          rfInstrIn.bits.isWritten := false.B
          rfInstrIn.valid := true.B
        }
      }
    } .otherwise {
      instrValidsRegIn(idx) := false.B
    }

    when (instrValidsReg(idx)) {

      io.branchPC.bits := instr.getBranchPC(instrReg.bits.word, opsReg)
      io.relativeBranch := instr.relativeBranch
      when (instr.branch) {
        when (io.relativeBranch) {
          io.branchPC.valid := (io.branchPC.bits =/= 0.S)
        } .otherwise {
          io.branchPC.valid := true.B
        }
      }
    }
  }

  when (instrIn.valid && !instrValid) {
    printf("decode: invalid instruction %x at PC %x\n", instrIn.bits.word, instrIn.bits.pc)
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
  pcWidth : Int,
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
    val instr = Input(new Instruction(iWidth, pcWidth))
    val instrValids = Input(Vec(numInstrs, Bool()))
    val instrReady = Output(Bool())
    val ops = Input(Vec(numOps, UInt(opWidth.W)))
    val results = Output(new ExecuteResults(dataWidth, addrWidth, rfDepth, numInstrs))
    val resultsReady = Input(Bool())
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
      results.readMem := instr.readMemory(io.instr.word)
      results.writeMem := instr.writeMemory(io.instr.word)
      results.writeRF := instr.writeRF(io.instr.word)
      when (results.readMem | results.writeMem | results.writeRF) {
        results.addr := instr.getAddress(io.instr.word, io.ops)
        results.rfIndex := instr.getWriteIndex(io.instr.word, io.ops)
        results.data := instr.getData(io.instr.word, io.instr.pc, io.ops)
      }
    }
    results.instrValids(idx) := io.instrValids(idx)
  }

  val resultsReg = RegNext(results)
  io.results := resultsReg

  io.instrReady := io.resultsReady
}

class MemoryModule(dataWidth : Int, addrWidth : Int, rfDepth : Int, instrs : Instructions) extends Module {

  val numInstrs = instrs.logic.size
  val rfIdxWidth = math.ceil(math.log(rfDepth)/math.log(2)).toInt

  val io = IO(new Bundle{
    val results = Input(new ExecuteResults(dataWidth, addrWidth, rfDepth, numInstrs))
    val resultsReady = Output(Bool())
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

  val stepLoad :: stepStore :: Nil = util.Enum(2)
  val stepReg = RegInit(stepLoad)
  val step = Wire(UInt(stepLoad.getWidth.W))
  step := stepLoad
  when (writeMem & readMem) {
    step := stepReg
  } .elsewhen (writeMem) {
    step := stepStore
  } .elsewhen (readMem) {
    step := stepLoad
  }

  io.memAddr.valid := writeMem | readMem
  io.memAddr.bits := addr
  io.memDataOut.valid := writeMem & (step === stepStore)
  io.memDataOut.bits := data
  io.memDataIn.ready := readMem & (step === stepLoad)

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
  io.resultsReady := !memBusy
  when (writeMem & readMem) {
    io.resultsReady := !memBusy & (step === stepStore)
  }

  when (memBusy) {
    writeMemReg := writeMem
    readMemReg := readMem
    writeRFReg := writeRF
    dataReg := data
    addrReg := addr
    rfIdxInReg := rfIdxIn
    instrValidsReg := instrValids
  }

  when (writeMem & readMem & !memBusy) {
    when (stepReg === stepStore) {
      stepReg := stepLoad
    } .otherwise {
      stepReg := stepReg + 1.U
    }
  }

  when (!memBusyReg) {
    writeMem := io.results.writeMem
    readMem := io.results.readMem
    writeRF := io.results.writeRF
    addr := io.results.addr
    rfIdxIn := io.results.rfIndex
    instrValids := io.results.instrValids
    data := io.results.data
    for ((instr, idx) <- instrs.logic.zipWithIndex) {
      when (instrValids(idx) & io.memDataIn.valid) {
        data := instr.getMemWriteData(io.results.data, io.memDataIn.bits)
      }
    }
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
