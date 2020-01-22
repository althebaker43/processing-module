
package ProcessingModule

import chisel3._

class DataIO[T <: Data, U <: Data](inValueType : T, outValueType : U) extends Bundle {
  val in = Flipped(util.Decoupled(inValueType))
  val out = outValueType
}

class AdderOutput(val dWidth : Int) extends Bundle {
  val memReq = util.Valid(UInt(4.W))
  val storeVal = util.Decoupled(UInt(dWidth.W))
}

class InstrIO(iWidth : Int) extends Bundle {
  val in = Flipped(util.Decoupled(UInt(iWidth.W)))
  val pc = util.Valid(UInt(64.W))
}

object AdderModule {

  val INSTR_NOP = 0
  val INSTR_INCR_1 = 1
  val INSTR_INCR_DATA = 2
  val INSTR_STORE = 3
  val INSTR_BGT = 4
}

object AdderInstruction {

  val codeNOP :: codeIncr1 :: codeIncrData :: codeStore :: codeBGT :: Nil = util.Enum(5)

  val codeWidth = codeNOP.getWidth

  val regWidth = 1

  val width = codeWidth + regWidth

  def apply(codeLit : Int, regLit : Int) : AdderInstruction = new AdderInstruction(codeLit.U, regLit.U)

  def nop : AdderInstruction = new AdderInstruction(codeNOP, 0.U)

  def incr1(regLit : Int) : AdderInstruction = new AdderInstruction(codeIncr1, regLit.U)

  def incrData(regLit : Int) : AdderInstruction = new AdderInstruction(codeIncrData, regLit.U)

  def store(regLit : Int) : UInt = (new AdderInstruction(codeStore, regLit.U)).asUInt

  def bgt(regLit : Int) : AdderInstruction = new AdderInstruction(codeBGT, regLit.U)

  def createInt(codeVal : UInt, regVal : UInt) : BigInt = (new AdderInstruction).createInt(codeVal, regVal)
}

class AdderInstruction(val code : UInt = UInt(AdderInstruction.codeWidth.W), val reg : UInt = UInt(AdderInstruction.regWidth.W)) extends Bundle {
  require(code.getWidth == AdderInstruction.codeWidth)
  require(reg.getWidth == AdderInstruction.regWidth)

  def createInt(codeVal : UInt, regVal : UInt) : BigInt = {
    def catValWidths(valWidthA : Tuple2[BigInt, Int], valWidthB : Tuple2[BigInt, Int]) : Tuple2[BigInt, Int] = {
      (((valWidthB._1 << valWidthA._2) | valWidthA._1), (valWidthA._2 + valWidthB._2))
    }
    ((((codeVal :: regVal :: Nil) map (_.litValue)) zip (getElements map (_.getWidth))) reduce catValWidths)._1
  }
}

class AdderModule(dWidth : Int, iWidth : Int, queueDepth : Int) extends Module {
  val io = IO(new Bundle {
    val data = new DataIO(UInt(dWidth.W), new AdderOutput(dWidth))
    val instr = new InstrIO(iWidth)
  });

  class InstrIODepend(val iWidth : Int) extends Bundle {
    val ioDepend = Bool()
    val instr = UInt(iWidth.W)
  }

  val STATE_INIT = 0
  val STATE_POP = 1
  val state = RegInit(STATE_INIT.U(1.W))

  val instrQueueIn = Wire(Flipped(util.Decoupled(new InstrIODepend(iWidth))))
  instrQueueIn.bits.ioDepend := (io.instr.in.bits === AdderInstruction.codeIncrData)
  instrQueueIn.bits.instr := io.instr.in.bits
  instrQueueIn.valid := io.instr.in.valid
  io.instr.in.ready := instrQueueIn.ready
  val instrQueue = util.Queue(instrQueueIn, queueDepth)

  val pcRegInitVal = Wire(util.Valid(UInt(64.W)))
  pcRegInitVal.bits := 0.U
  pcRegInitVal.valid := true.B
  val pcReg = RegInit(pcRegInitVal)
  io.instr.pc := pcReg

  val reg = RegInit(0.U(dWidth.W))
  io.data.out.storeVal.bits := reg

  val curInstrDepend = Reg(util.Valid(new InstrIODepend(iWidth)))

  val curAdderInstr = Wire(new AdderInstruction)
  curAdderInstr.code := curInstrDepend.bits.instr(2,0)
  curAdderInstr.reg := curInstrDepend.bits.instr(3)

  val storeValReg = RegInit(false.B)
  when (storeValReg) {
    storeValReg := false.B
  }
  io.data.out.storeVal.valid := storeValReg

  val memReqReg = RegInit(0.U(4.W))
  io.data.out.memReq.bits := memReqReg
  io.data.out.memReq.valid := (memReqReg != 0.U) 

  val dataInQueue = util.Queue(io.data.in, 3)
  val dataReadyReg = RegInit(false.B)
  dataInQueue.ready := dataReadyReg

  val popReg = RegInit(false.B)
  when (popReg) {
    popReg := false.B
  }

  instrQueue.ready := (state === STATE_POP.U)

  //io.data.out.valid := io.data.out.memReq.valid | io.data.out.storeVal.valid

  val memStateInit :: memStateWait :: Nil = util.Enum(2)
  val memStateReg = RegInit(memStateInit)

  when (state === STATE_INIT.U){
    when (curInstrDepend.valid) {
      when (curInstrDepend.bits.ioDepend) {

        when (memStateReg === memStateInit) {

          memReqReg := 1.U
          pcReg.valid := false.B
          dataReadyReg := true.B
          memStateReg := memStateWait

        } .elsewhen (memStateReg === memStateWait) {

          when (dataInQueue.valid) {

            dataReadyReg := false.B
            memStateReg := memStateInit
            reg := reg + dataInQueue.bits

            memReqReg := 0.U

            state := STATE_POP.U
            popReg := true.B
            pcReg.bits := pcReg.bits + 1.U
            pcReg.valid := true.B
          }
        }
      } .otherwise {

        when (curAdderInstr.code === AdderInstruction.codeStore) {
          when (io.data.out.storeVal.ready) {
            when (!storeValReg) {
              storeValReg := true.B
            } .otherwise {
              state := STATE_POP.U
              popReg := true.B
              pcReg.bits := pcReg.bits + 1.U
              pcReg.valid := true.B
            }
          }
        } .otherwise {

          when ((curAdderInstr.code === AdderInstruction.codeBGT) && (reg > 0.U)) {
            pcReg.bits := pcReg.bits + 2.U
          } .otherwise {
            pcReg.bits := pcReg.bits + 1.U
          }
          pcReg.valid := true.B

          state := STATE_POP.U
          popReg := true.B
        }
      }
    } .otherwise {
      pcReg.valid := false.B
      state := STATE_POP.U
      popReg := true.B
    }
  } .elsewhen (state === STATE_POP.U) {
    when (instrQueue.valid) {
      state := STATE_INIT.U
      curInstrDepend.bits := instrQueue.bits
      curInstrDepend.valid := true.B
      pcReg.valid := false.B
    } .otherwise {
      curInstrDepend.bits.ioDepend := false.B
      val nopInstr = Wire(new AdderInstruction)
      nopInstr.code := AdderInstruction.codeNOP
      nopInstr.reg := 0.U(1.W)
      curInstrDepend.bits.instr := nopInstr.asUInt
      curInstrDepend.valid := false.B
      pcReg.valid := !io.instr.in.valid
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
