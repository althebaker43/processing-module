
package ProcessingModule

import chisel3._
import scala.math

class ReorderQueue[U <: Data](val gen : U, depth : Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(util.Decoupled(gen))
    val enqReady = Input(Bool())
    val data = Output(Vec(depth, gen))
    val dataReady = Input(Vec(depth, Bool()))
    val deq = util.Decoupled(gen)
  })

  val idxWidth = math.ceil(math.log(depth)/math.log(2)).toInt

  val regs = Reg(Vec(depth, util.Valid(gen)))
  val tail = RegInit(~(0.U(idxWidth.W)))
  val head = RegInit(~(0.U(idxWidth.W)))
  val deqReg = Reg(util.Valid(gen))

  io.deq.valid := deqReg.valid
  io.deq.bits := deqReg.bits

  val full = RegInit(false.B)
  val empty = tail === head

  val offset = Wire(UInt(idxWidth.W))
  offset := depth.U - 1.U - head

  // Compute index of first ready element
  val noneReady = Wire(util.Valid(UInt(idxWidth.W)))
  noneReady.valid := false.B
  noneReady.bits := 0.U
  val allReadyIdxs = Wire(Vec(depth, util.Valid(UInt(idxWidth.W))))
  for (i <- 0 until depth) {
    allReadyIdxs(i).valid := !empty
    allReadyIdxs(i).bits := i.U
  }
  val readyIdx = util.MuxCase(noneReady, Array.tabulate(depth){
    (i : Int) => (regs(head - i.U).valid -> allReadyIdxs(head - i.U))
  })

  io.enq.ready := !full || (io.deq.ready && readyIdx.valid)

  for (i <- 0 until depth) {
    io.data(i) := regs(i).bits
  }

  val addElement = ~(empty && io.enqReady) && io.enq.ready && io.enq.valid
  val removeElement = io.deq.ready && !empty && readyIdx.valid

  val updateDeq = removeElement || (io.deq.ready && io.enq.valid && io.enqReady)
  deqReg.valid := updateDeq

  // Data register update
  val inRange = Wire(Vec(depth, Bool()))
  for (i <- 0 until depth) {

    inRange(i) := ((i.U + offset) >= (tail + offset)) && ((i.U + offset) <= ~(0.U(idxWidth.W)))

    when (inRange(i)) {
      when (addElement) {
        when (i.U === tail) {
          regs(i).bits := io.enq.bits
          regs(i).valid := io.enqReady
        }
      } .elsewhen (removeElement) {
        when ((i.U + offset) < (depth.U - 1.U)) {
          when ((i.U + offset) >= (readyIdx.bits + offset)) {
            if (i < (depth - 1)) {
              regs(i).bits := regs(i+1).bits
              regs(i).valid := io.dataReady(i+1)
            }
            else {
              regs(i).bits := regs(0).bits
              regs(i).valid := io.dataReady(0)
            }
          }
        } .otherwise {
          regs(i).valid := false.B
        }
      } .otherwise {
        regs(i).valid := io.dataReady(i)
      }
    } .otherwise {
      regs(i).valid := false.B
    }
  }

  // Head pointer update
  when (removeElement) {
    head := head - 1.U
  }

  // Tail pointer update
  when (!full) {
    when (addElement && (tail - 1.U =/= head)) {
      tail := tail - 1.U
    }
  } .otherwise {
    when (addElement || removeElement) {
      tail := tail - 1.U
    }
  }

  // Full flag update 
  when (addElement && (tail - 1.U === head)) {
    full := true.B
  } .elsewhen (removeElement && !addElement) {
    full := false.B
  }

  // Dequeue register update
  when (updateDeq) {
    when (readyIdx.valid) {
      deqReg.bits := regs(readyIdx.bits).bits
    } .elsewhen (io.enqReady && io.enq.valid) {
      deqReg.bits := io.enq.bits
    }
  }
}

object ReorderQueue {
  def apply[U <: Data](enq : util.DecoupledIO[U], depth : Int, isReady : (U) => Bool) : util.DecoupledIO[U] = {
    val queue = Module(new ReorderQueue[U](chiselTypeOf(enq.bits), depth))
    queue.io.enq.valid := enq.valid
    queue.io.enq.bits := enq.bits
    queue.io.enqReady := isReady(enq.bits)
    for (i <- 0 until depth) {
      queue.io.dataReady(i) := isReady(queue.io.data(i))
    }
    enq.ready := queue.io.enq.ready
    util.TransitName(queue.io.deq, queue)
  }
}
