
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
  val INSTR_STORE = 3
}

class AdderModule(dWidth : Int) extends Module {
  val io = IO(new Bundle {
    val data = new DataIO(UInt(dWidth.W), new AdderOutput(dWidth))
    val instr = Flipped(util.Decoupled(UInt(3.W)))
  });

  io.data.out.bits.memReq.valid := false.B
  io.data.out.bits.memReq.bits := false.B
  io.data.out.valid := io.data.out.bits.memReq.valid | io.data.out.bits.storeVal.valid
  io.data.in.ready := true.B
  io.instr.ready := true.B

  val reg = RegInit(0.U(dWidth.W))
  io.data.out.bits.storeVal.bits := reg

  val storeValReg = RegInit(false.B)
  when (storeValReg === true.B) {
    storeValReg := false.B
  }
  io.data.out.bits.storeVal.valid := storeValReg

  when (io.instr.valid) {
    when (io.instr.bits === AdderModule.INSTR_INCR_1.U) {
      reg := reg + 1.U
    } .elsewhen (io.instr.bits === AdderModule.INSTR_STORE.U) {
      storeValReg := true.B
    }
  }
}
