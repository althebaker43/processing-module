
package ProcessingModule

import chisel3._
import scala.math

class ReorderQueue[U <: Data](val gen : U, depth : Int, isReady : (U) => Bool) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(util.Decoupled(gen))
    val deq = util.Decoupled(gen)
  })

  val idxWidth = math.ceil(math.log(depth)/math.log(2)).toInt

  val regs = Reg(Vec(depth, util.Valid(gen)))
  val tail = RegInit(depth.U - 1.U)
  val head = RegInit(depth.U - 1.U)
  val deqReg = Reg(util.Valid(gen))

  val empty = tail === head
  val full = tail - 1.U === head
  val offset = depth.U - 1.U - head

  io.enq.ready := !full
  io.deq.valid := deqReg.valid
  io.deq.bits := deqReg.bits

  // Compute index of first ready element
  val noneReady = Wire(util.Valid(UInt(idxWidth.W)))
  noneReady.valid := false.B
  val allReadyIdxs = Wire(Vec(depth, util.Valid(UInt(idxWidth.W))))
  for (i <- 0 until depth) {
    allReadyIdxs(i).valid := !empty
    allReadyIdxs(i).bits := i.U
  }
  val readyIdx = util.MuxCase(noneReady, Array.tabulate(depth){
    (i : Int) => (regs(head - i.U).valid -> allReadyIdxs(head - i.U))
  })

  // Data register update
  for (i <- 0 until depth) {
    when (((i.U + offset) >= (tail + offset)) && ((i.U + offset) <= (depth.U - 1.U))) {
      when (io.enq.valid && !full) {
        when (readyIdx.valid && (i.U === tail)) {
          regs(i).bits := io.enq.bits
          regs(i).valid := isReady(io.enq.bits)
        }
      } .elsewhen (io.deq.ready && readyIdx.valid) {
        if (i < (depth - 1)) {
          when ((i.U + offset) >= (readyIdx.bits + offset)) {
            regs(i).bits := regs(i+1).bits
            regs(i).valid := isReady(regs(i+1).bits)
          }
        }
      } .otherwise {
        regs(i).valid := isReady(regs(i).bits)
      }
    }
  }

  // Head pointer update
  when (io.deq.ready && readyIdx.valid) {
    head := head - 1.U
  }

  // Tail pointer update
  when (io.enq.valid && !readyIdx.valid && !io.deq.ready) {
    tail := tail - 1.U
  }

  // Dequeue register update
  when (io.deq.ready) {
    when (readyIdx.valid) {
      deqReg.bits := regs(readyIdx.bits).bits
      deqReg.valid := true.B
    } .elsewhen (io.enq.valid && isReady(io.enq.bits)) {
      deqReg.bits := io.enq.bits
      deqReg.valid := true.B
    }
  } .otherwise {
    deqReg.valid := false.B
  }
}

object ReorderQueue {
  def apply[U <: Data](enq : util.DecoupledIO[U], depth : Int, isReady : (U) => Bool) : util.DecoupledIO[U] = {
    val queue = Module(new ReorderQueue[U](chiselTypeOf(enq.bits), depth, isReady))
    queue.io.enq.valid := enq.valid
    queue.io.enq.bits := enq.bits
    enq.ready := queue.io.enq.ready
    util.TransitName(queue.io.deq, queue)
  }
}
