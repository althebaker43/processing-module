
package ProcessingModule

import chisel3._

class DataIO[T <: Data, U <: Data](inValueType : T, outValueType : U) extends Bundle {
  val in = Flipped(util.Decoupled(inValueType))
  val out = util.Decoupled(outValueType)
}

class AdderOutput(val dWidth : Int) extends Bundle {
  val memReq = util.Valid(Bool())
  val storeVal = util.Valid(UInt(dWidth.W))
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
  instrQueueIn.bits.ioDepend := (io.instr.in.bits === AdderModule.INSTR_INCR_DATA.U)
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
  io.data.out.bits.storeVal.bits := reg

  val curInstrDepend = Reg(util.Valid(new InstrIODepend(iWidth)))

  val storeValReg = RegInit(false.B)
  when (storeValReg) {
    storeValReg := false.B
  }
  io.data.out.bits.storeVal.valid := storeValReg

  val memReqReg = RegInit(false.B)
  io.data.out.bits.memReq.bits := memReqReg
  io.data.out.bits.memReq.valid := io.data.out.bits.memReq.bits
  io.data.in.ready := memReqReg

  val popReg = RegInit(false.B)
  when (popReg) {
    popReg := false.B
  }
  instrQueue.ready := popReg

  io.data.out.valid := io.data.out.bits.memReq.valid | io.data.out.bits.storeVal.valid

  when (state === STATE_INIT.U){
    when (curInstrDepend.valid) {
      when (curInstrDepend.bits.ioDepend) {
        when (memReqReg) {
          when (io.data.in.valid) {

            reg := reg + io.data.in.bits

            memReqReg := false.B
            state := STATE_POP.U
            pcReg.bits := pcReg.bits + 1.U
            pcReg.valid := true.B
          }
        } .otherwise {
          memReqReg := true.B
          pcReg.valid := false.B
        }
      } .otherwise {

        when (curInstrDepend.bits.instr === AdderModule.INSTR_INCR_1.U) {
          reg := reg + 1.U
        } .elsewhen (curInstrDepend.bits.instr === AdderModule.INSTR_STORE.U) {
          storeValReg := true.B
        }

        state := STATE_POP.U

        when ((curInstrDepend.bits.instr === AdderModule.INSTR_BGT.U) && (reg > 0.U)) {
          pcReg.bits := pcReg.bits + 2.U
        } .otherwise {
          pcReg.bits := pcReg.bits + 1.U
        }
        pcReg.valid := true.B
      }
    } .otherwise {
      pcReg.valid := false.B
      state := STATE_POP.U
    }
  } .elsewhen (state === STATE_POP.U) {
    when (instrQueue.valid) {
      popReg := true.B
      state := STATE_INIT.U
      curInstrDepend.bits := instrQueue.bits
      curInstrDepend.valid := true.B
    } .otherwise {
      curInstrDepend.bits.ioDepend := false.B
      curInstrDepend.bits.instr := AdderModule.INSTR_NOP.U
      curInstrDepend.valid := false.B
    }
    pcReg.valid := false.B
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
