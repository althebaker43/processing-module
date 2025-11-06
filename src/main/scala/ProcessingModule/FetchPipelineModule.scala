package ProcessingModule

import chisel3._

class FetchPipelineModule(iWidth : Int, pcWidth : Int, pcAlign : Int, val dbgMsg : Boolean = true) extends Module {

  val io = IO(new Bundle {
    val branchPCIn = Flipped(util.Valid(SInt(pcWidth.W)))
    val pcOut = util.Valid(UInt(pcWidth.W))
    val memInstr = Flipped(util.Decoupled(UInt(iWidth.W)))
    val instr = util.Decoupled(new Instruction(iWidth, pcWidth))
  })

  val pcReg = RegInit(0.U(iWidth.W))

  val pcQueue = Module(new ClearableQueue(UInt(iWidth.W), depth=2))

  val outQueue = Module(new ClearableQueue(new Instruction(iWidth, pcWidth), depth=2))

  val pcRegValid = Wire(Bool())
  pcRegValid := !io.branchPCIn.valid & pcQueue.io.enq.ready & outQueue.io.enq.ready

  when (io.branchPCIn.valid) {
    pcReg := io.branchPCIn.bits.asUInt()
  } .elsewhen (pcRegValid) {
    pcReg := pcReg + (1.U << (pcAlign - 1))
  }

  pcQueue.io.clear := io.branchPCIn.valid
  pcQueue.io.enq.bits := pcReg
  pcQueue.io.enq.valid := pcRegValid
  pcQueue.io.deq.ready := io.memInstr.valid & outQueue.io.enq.ready

  outQueue.io.clear := io.branchPCIn.valid
  outQueue.io.enq.bits.pc := pcQueue.io.deq.bits
  outQueue.io.enq.bits.word := io.memInstr.bits
  outQueue.io.enq.valid := pcQueue.io.deq.valid & io.memInstr.valid
  outQueue.io.deq.ready := io.instr.ready

  io.pcOut.bits := pcReg
  io.pcOut.valid := pcRegValid

  io.memInstr.ready := !io.branchPCIn.valid & outQueue.io.enq.ready

  io.instr.valid := outQueue.io.deq.valid
  io.instr.bits := outQueue.io.deq.bits
}
