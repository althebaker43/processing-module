package ProcessingModule

import chisel3._
import chisel3.util.{QueueIO, log2Ceil}

class ClearableQueueIO[T <: Data](gen : T, val depth : Int) extends QueueIO(gen, depth) {
  val clear = Input(Bool())
}

class ClearableQueue[T <: Data](gen : T, depth : Int, val dbgMsg : Boolean = true) extends Module {

  val io = IO(new ClearableQueueIO(gen, depth))

  val enqReg = Reg(gen)
  enqReg := io.enq.bits

  val idxWidth = log2Ceil(depth)
  val countWidth = log2Ceil(depth+1)

  val regs = Reg(Vec(depth, gen))

  val head = RegInit(0.U(idxWidth.W))
  val nextHead = Wire(UInt(idxWidth.W))
  when ((head === (depth-1).U) || io.clear) {
    nextHead := 0.U
  } .otherwise {
    nextHead := head + 1.U
  }
  val headPhase = RegInit(false.B)

  val tail = RegInit(0.U(idxWidth.W))
  val nextTail = Wire(UInt(idxWidth.W))
  when ((tail === (depth-1).U) || io.clear) {
    nextTail := 0.U
  } .otherwise {
    nextTail := tail + 1.U
  }
  val tailPhase = RegInit(false.B)

  io.enq.ready := (head =/= tail) || !(headPhase ^ tailPhase) || io.deq.valid
  val validEnq = Wire(Bool())
  validEnq := false.B
  when (io.enq.valid && io.enq.ready && !io.clear) {
    validEnq := true.B
  }

  val validDeqReg = RegInit(false.B)
  val validDeq = Wire(Bool())
  validDeq := false.B
  validDeqReg := validDeq
  io.deq.valid := validDeqReg
  when (io.deq.ready && (((nextTail =/= head) && (tail =/= head)) || (headPhase ^ tailPhase)) && !io.clear) {
    validDeq := true.B
  }

  when (io.clear) {
    head := 0.U
    headPhase := false.B
  } .elsewhen (validEnq) {
    regs(head) := io.enq.bits
    head := nextHead
    when (nextHead === 0.U) {
      headPhase := ~headPhase
    }
  }

  io.deq.bits := regs(tail)
  when (io.clear) {
    tail := 0.U
    tailPhase := false.B
  } .elsewhen (validDeqReg && !io.clear) {
    tail := nextTail
    when (nextTail === 0.U) {
      tailPhase := ~tailPhase
    }
  }

  val count = RegInit(0.U(countWidth.W))
  io.count := count
  when (io.clear) {
    count := 0.U
  } .elsewhen (validEnq && !io.deq.valid) {
    count := count + 1.U
  } .elsewhen (!validEnq && io.deq.valid) {
    count := count - 1.U
  }

  when (io.clear) {
    for (i <- 0 until depth) {
      regs(i) := 0.U
    }
  }
}
