
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

object AdderModule {

  val INSTR_NOP = 0
  val INSTR_INCR_1 = 1
  val INSTR_INCR_DATA = 2
  val INSTR_STORE = 3
}

class AdderModule(dWidth : Int, iWidth : Int, queueDepth : Int) extends Module {
  val io = IO(new Bundle {
    val data = new DataIO(UInt(dWidth.W), new AdderOutput(dWidth))
    val instr = Flipped(util.Decoupled(UInt(iWidth.W)))
  });

  class InstrIODepend(val iWidth : Int) extends Bundle {
    val ioDepend = Bool()
    val instr = UInt(iWidth.W)
  }

  val reg = RegInit(0.U(dWidth.W))
  io.data.out.bits.storeVal.bits := reg

  val STATE_INIT = 0
  val STATE_POP = 1
  val state = RegInit(STATE_INIT.U(1.W))

  val instrQueueIn = Flipped(util.Decoupled(new InstrIODepend(iWidth)))
  instrQueueIn.bits.ioDepend := (io.instr.bits === AdderModule.INSTR_INCR_DATA.U)
  instrQueueIn.bits.instr := io.instr.bits
  instrQueueIn.valid := io.instr.valid
  io.instr.ready := instrQueueIn.ready
  val instrQueue = util.Queue(instrQueueIn, queueDepth)
  instrQueue.ready := (state === STATE_POP.U)

  val curInstrDepend = Reg(new InstrIODepend(iWidth))
  when (instrQueue.valid) {
    curInstrDepend := instrQueue.bits
  } .otherwise {
    curInstrDepend.ioDepend := false.B
    curInstrDepend.instr := AdderModule.INSTR_NOP.U
  }

  val storeValReg = RegInit(false.B)
  when (storeValReg) {
    storeValReg := false.B
  }
  io.data.out.bits.storeVal.valid := storeValReg

  io.data.out.bits.memReq.bits := curInstrDepend.ioDepend
  io.data.out.bits.memReq.valid := io.data.out.bits.memReq.bits
  io.data.out.valid := io.data.out.bits.memReq.valid | io.data.out.bits.storeVal.valid
  io.data.in.ready := curInstrDepend.ioDepend

  when (state === STATE_INIT.U){
    when (curInstrDepend.ioDepend) {
      when (io.data.in.valid) {

        reg := reg + io.data.in.bits

        state := STATE_POP.U
      }
    } .otherwise {

      when (curInstrDepend.instr === AdderModule.INSTR_INCR_1.U) {
        reg := reg + 1.U
      } .elsewhen (curInstrDepend.instr === AdderModule.INSTR_STORE.U) {
        storeValReg := true.B
      }

      state := STATE_POP.U
    }
  } .elsewhen (state === STATE_POP.U) {
    when (instrQueue.valid) {
      state := STATE_INIT.U
    }
  }
}
