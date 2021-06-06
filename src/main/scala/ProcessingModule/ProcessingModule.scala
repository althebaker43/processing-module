
package ProcessingModule

import chisel3._

abstract class Instructions {

  def logic : Seq[InstructionLogic]
}

abstract class InstructionLogic(val name : String, val dataInDepend : Boolean, val dataOutDepend : Boolean) {

  def decode( instr : UInt) : Bool

  def getRFIndex(instr : UInt, opIndex : Int) : UInt = 0.U

  def execute( instr : UInt) : Unit

  def load(instr : UInt) : UInt = 0.U

  def store(instr : UInt) : UInt = 0.U
}

abstract class ProcessingModule(dWidth : Int, dAddrWidth : Int, iWidth : Int, queueDepth : Int) extends Module {

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

  class InstrIODepend(val iWidth : Int) extends Bundle {
    val ioDepend = Bool()
    val instr = UInt(iWidth.W)
  }

  val instrs = initInstrs

  val stateInit :: statePop :: Nil = util.Enum(2)
  val state = RegInit(stateInit)

  val instrQueueIn = Wire(Flipped(util.Decoupled(new InstrIODepend(iWidth))))
  instrQueueIn.bits.ioDepend := false.B
  for (instr <- instrs.logic if instr.dataInDepend) {
    when (instr.decode(io.instr.in.bits)) {
      instrQueueIn.bits.ioDepend := true.B
    }
  }
  instrQueueIn.bits.instr := io.instr.in.bits
  instrQueueIn.valid := io.instr.in.valid
  io.instr.in.ready := instrQueueIn.ready
  val instrQueue = util.Queue(instrQueueIn, queueDepth)

  val pcRegInitVal = Wire(util.Valid(UInt(64.W)))
  pcRegInitVal.bits := 0.U
  pcRegInitVal.valid := true.B
  val pcReg = RegInit(pcRegInitVal)
  io.instr.pc := pcReg

  io.data.out.value.bits := 0.U

  val curInstrDepend = Reg(util.Valid(new InstrIODepend(iWidth)))

  val dataOutValid = RegInit(false.B)
  val dataOut = RegInit(0.U(dWidth.W))
  io.data.out.value.valid := dataOutValid
  io.data.out.value.bits := dataOut

  val dAddrInitVal = Wire(util.Valid(UInt(dAddrWidth.W)))
  dAddrInitVal.bits := 0.U
  dAddrInitVal.valid := false.B
  val dAddrReg = RegInit(dAddrInitVal)
  io.data.out.addr := dAddrReg

  val dataInQueue = util.Queue(io.data.in, 3)
  val dataReadyReg = RegInit(false.B)
  dataInQueue.ready := dataReadyReg

  val dataIn = Wire(UInt(dWidth.W))
  dataIn := dataInQueue.bits

  val popReg = RegInit(false.B)
  when (popReg) {
    popReg := false.B
  }

  instrQueue.ready := (state === statePop)

  val memStateInit :: memStateWait :: Nil = util.Enum(2)
  val memStateReg = RegInit(memStateInit)

  val outputDepend = Wire(Bool())
  outputDepend := false.B
  for (instr <- instrs.logic if instr.dataOutDepend) {
    when (instr.decode(curInstrDepend.bits.instr)) {
      outputDepend := true.B
    }
  }

  when (state === stateInit){
    when (curInstrDepend.valid) {
      when (curInstrDepend.bits.ioDepend) {

        when (memStateReg === memStateInit) {

          dAddrReg.valid := true.B
          for (instr <- instrs.logic if instr.dataInDepend) {
            when (instr.decode(curInstrDepend.bits.instr)) {
              dAddrReg.bits := instr.load(curInstrDepend.bits.instr)
            }
          }

          pcReg.valid := false.B
          dataReadyReg := true.B
          memStateReg := memStateWait

        } .elsewhen (memStateReg === memStateWait) {

          when (dataInQueue.valid) {

            dataReadyReg := false.B
            memStateReg := memStateInit

            for (instr <- instrs.logic if instr.dataInDepend) {
              when (instr.decode(curInstrDepend.bits.instr)) {
                instr.execute(curInstrDepend.bits.instr)
              }
            }

            dAddrReg.valid := false.B

            state := statePop
            popReg := true.B
            pcReg.bits := pcReg.bits + 1.U
            pcReg.valid := true.B
          }
        }
      } .otherwise {

        when (outputDepend) {
          when (io.data.out.value.ready) {
            when (!dataOutValid) {

              dAddrReg.valid := true.B
              for (instr <- instrs.logic if instr.dataOutDepend) {
                when (instr.decode(curInstrDepend.bits.instr)) {
                  dAddrReg.bits := instr.store(curInstrDepend.bits.instr)
                }
              }

              dataOutValid := true.B
              for (instr <- instrs.logic if instr.dataOutDepend) {
                when (instr.decode(curInstrDepend.bits.instr)) {
                  instr.execute(curInstrDepend.bits.instr)
                }
              }
            } .otherwise {

              dAddrReg.valid := false.B
              dataOutValid := false.B
              state := statePop
              popReg := true.B
              pcReg.bits := pcReg.bits + 1.U
              pcReg.valid := true.B
            }
          }
        } .otherwise {

          pcReg.bits := pcReg.bits + 1.U
          for (instr <- instrs.logic) {
            when (instr.decode(curInstrDepend.bits.instr)) {
              instr.execute(curInstrDepend.bits.instr)
            }
          }

          pcReg.valid := true.B
          state := statePop
          popReg := true.B
        }
      }
    } .otherwise {
      pcReg.valid := false.B
      state := statePop
      popReg := true.B
    }
  } .elsewhen (state === statePop) {
    when (instrQueue.valid) {
      state := stateInit
      curInstrDepend.bits := instrQueue.bits
      curInstrDepend.valid := true.B
      pcReg.valid := false.B
    } .otherwise {
      curInstrDepend.bits.ioDepend := false.B
      curInstrDepend.valid := false.B
      pcReg.valid := !io.instr.in.valid
    }
  }
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

class AdderModule(dWidth : Int) extends ProcessingModule(dWidth, AdderInstruction.addrWidth, AdderInstruction.width, 3) {

  def getInstrCode(instr : UInt) : UInt = instr(2,0)
  def getInstrReg(instr : UInt) : UInt = instr(3)
  def getInstrAddr(instr : UInt) : UInt = instr(7,4)

  def initInstrs = new Instructions {

    val regs = RegInit(VecInit(Seq.fill(2){ 0.U(dWidth.W) }))

    def logic = {
      new InstructionLogic("nop", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool = getInstrCode(instr) === AdderInstruction.codeNOP
        def execute ( instr : UInt ) : Unit = Unit
      } ::
      new InstructionLogic("incr1", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool = {
          getInstrCode(instr) === AdderInstruction.codeIncr1
        }
        def execute ( instr : UInt ) : Unit = {
          regs(getInstrReg(instr)) := regs(getInstrReg(instr)) + 1.U
        }
      } ::
      new InstructionLogic("incrData", dataInDepend=true, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool = {
          getInstrCode(instr) === AdderInstruction.codeIncrData
        }
        override def load ( instr : UInt ) : UInt = getInstrAddr(instr)
        def execute ( instr : UInt ) : Unit = {
          regs(getInstrReg(instr)) := regs(getInstrReg(instr)) + dataIn
        }
      } ::
      new InstructionLogic("store", dataInDepend=false, dataOutDepend=true) {
        def decode ( instr : UInt ) : Bool = {
          getInstrCode(instr) === AdderInstruction.codeStore
        }
        override def store ( instr : UInt ) : UInt = getInstrAddr(instr)
        def execute ( instr : UInt ) : Unit = {
          dataOut := regs(getInstrReg(instr))
        }
      } ::
      new InstructionLogic("bgt", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool = {
          getInstrCode(instr) === AdderInstruction.codeBGT
        }
       def execute ( instr : UInt ) : Unit = {
          when ( regs(getInstrReg(instr)) > 0.U ) { pcReg.bits := pcReg.bits + 2.U }
        }
      } ::
      Nil
    }
  }
}

class FetchModule(iWidth : Int) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(UInt(64.W)))
    val pcOut = util.Valid(UInt(64.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(UInt(iWidth.W))
  })

  val pc = RegInit(0.U(64.W))
  when (io.instr.ready && io.memInstr.valid) {
    when (io.branchPCIn.valid) {
      pc := io.branchPCIn.bits
    }
      .otherwise {
        pc := pc + 1.U
      }
  }

  io.pcOut.bits := pc
  io.pcOut.valid := io.instr.ready

  io.instr <> io.memInstr
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

  val io = IO(new Bundle {
    val instr = Flipped(util.Valid(UInt(iWidth.W)))
    val instrValids = Output(Vec(numInstrs, Bool()))
    val ops = Output(Vec(numOps, UInt(opWidth.W)))
  })

  val instrValidsReg = Reg(Vec(numInstrs, Bool()))
  val instrValidsRegIn = Wire(Vec(numInstrs, Bool()))
  for (idx <- 0 until numInstrs) {
    instrValidsReg(idx) := instrValidsRegIn(idx)
    io.instrValids(idx) := instrValidsReg(idx)
  }

  val rf = VecInit(Seq.fill(rfDepth){ RegInit(0.U(rfWidth.W)) })

  val opsReg = Reg(Vec(numOps, UInt(opWidth.W)))
  io.ops := opsReg

  for ((instr, idx) <- instrs.logic.zipWithIndex) {
    when (io.instr.valid) {
      instrValidsRegIn(idx) := instr.decode(io.instr.bits)
      when (instrValidsRegIn(idx)) {
        for (opIdx <- 0 until numOps) {
          opsReg(opIdx) := rf(instr.getRFIndex(io.instr.bits, opIdx))
        }
      }
    } .otherwise {
      instrValidsRegIn(idx) := false.B
    }
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
