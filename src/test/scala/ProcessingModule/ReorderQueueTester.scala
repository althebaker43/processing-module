
package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.ChiselFlatSpec

class UIntReorderQueue(dWidth : Int, depth : Int) extends Module {

  val io = IO(new Bundle {
    val enq = Flipped(util.Decoupled(UInt(dWidth.W)))
    val deq = util.Decoupled(UInt(dWidth.W))
    val readyEnq = Flipped(util.Decoupled(UInt(dWidth.W)))
    val readyDeq = util.Decoupled(UInt(dWidth.W))
  })

  io.deq <> ReorderQueue[UInt](io.enq, depth, (i : UInt) => {
    io.readyDeq.valid && (i === io.readyDeq.bits)
  })

  io.readyDeq <> util.Queue(io.readyEnq)
}

class ReorderQueueTester extends ChiselFlatSpec {

  val dWidth = 4
  val depth = 4

  behavior of "ReorderQueue"

  it should "dequeue the first valid element" in {
    assertTesterPasses {
      new DecoupledTester("dequeue") {
        val dut = Module(new UIntReorderQueue(dWidth, depth))
        val events = new InputEvent((dut.io.enq, 1)) ::
        new InputEvent((dut.io.enq, 2)) ::
        new InputEvent((dut.io.enq, 3)) ::
        new InputEvent((dut.io.enq, 4)) ::
        new InputEvent((dut.io.readyEnq, 4)) ::
        new InputEvent((dut.io.readyEnq, 2)) ::
        new InputEvent((dut.io.readyEnq , 3)) ::
        new InputEvent((dut.io.readyEnq , 1)) ::
        new OutputEvent((dut.io.deq , 4)) ::
        new OutputEvent((dut.io.deq , 2)) ::
        new OutputEvent((dut.io.deq , 3)) ::
        new OutputEvent((dut.io.deq , 1)) ::
        new OutputEvent((dut.io.readyDeq , 4)) ::
        new OutputEvent((dut.io.readyDeq , 2)) ::
        new OutputEvent((dut.io.readyDeq , 3)) ::
        new OutputEvent((dut.io.readyDeq , 1)) ::
        Nil
      }
    }
  }
}
