package ProcessingModule.RISCV

import chisel3._

class RISCVLoaderModule(dumpPath : String) extends Module {

  val io = IO(new Bundle {
    val status = util.Valid(Bool())
  })

  io.status.valid := false.B
  io.status.bits := false.B
}
