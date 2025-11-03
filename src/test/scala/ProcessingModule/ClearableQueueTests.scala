package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}
import chisel3.testers.TesterDriver

class ClearableQueueTester(dut : ClearableQueue[UInt]) extends PeekPokeTester[ClearableQueue[UInt]](dut) {

  implicit def intToSignalValue(intVal : Int) : SignalValue = new SignalValue(isValid=true, value=intVal)

  val u = new SignalValue(isValid=false)
  val x = new SignalValue(isValid=false)

  def stepUpdate(enqV : SignalValue,
    enq : SignalValue,
    deqR : SignalValue,
    clear : SignalValue,
    enqR : SignalValue,
    deqV : SignalValue,
    deq : SignalValue) = {
    step(1)
    if (enqV.isValid) poke(dut.io.enq.valid, enqV.value)
    if (enq.isValid) poke(dut.io.enq.bits, enq.value)
    if (deqR.isValid) poke(dut.io.deq.ready, deqR.value)
    if (clear.isValid) poke(dut.io.clear, clear.value)
    if (enqR.isValid) expect(dut.io.enq.ready, enqR.value)
    if (deqV.isValid) expect(dut.io.deq.valid, deqV.value)
    if (deq.isValid) expect(dut.io.deq.bits, deq.value)
  }
}

class ClearableQueueTests extends ChiselFlatSpec {

  val width = 4

  def executeTest(testName : String, depth : Int)(testerGen : ClearableQueue[UInt] => PeekPokeTester[ClearableQueue[UInt]]) : Boolean = Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/clearQueue_" + testName), () => new ClearableQueue(UInt(width.W), depth))(testerGen)

  behavior of "ClearableQueue"

  it should "enqueue and dequeue" in {
    executeTest("basic", depth=4){
      dut : ClearableQueue[UInt] => new ClearableQueueTester(dut){

        poke(dut.io.enq.valid, 0)
        poke(dut.io.enq.bits, 0)
        poke(dut.io.deq.ready, 0)
        poke(dut.io.clear, 0)

        stepUpdate(enqV=1, enq=1, deqR=u, clear=u, enqR=1, deqV=0, deq=x) // Step 1
        stepUpdate(     1,     2,      u,       u,      1,      0,     x) //      2
        stepUpdate(     0,     3,      u,       u,      1,      0,     x) //      3
        stepUpdate(     0,     3,      1,       u,      1,      0,     x) //      4
        stepUpdate(     0,     3,      u,       u,      1,      1,     1) //      5
        stepUpdate(     0,     3,      u,       u,      1,      1,     2) //      6
        stepUpdate(     0,     3,      u,       u,      1,      0,     x) //      7
        stepUpdate(     0,     3,      u,       u,      1,      0,     x) //      8
      }
    } should be (true)
  }

  it should "circle around" in {
    executeTest("circle", depth=4){
      dut : ClearableQueue[UInt] => new ClearableQueueTester(dut){

        poke(dut.io.enq.valid, 0)
        poke(dut.io.enq.bits, 0)
        poke(dut.io.deq.ready, 0)
        poke(dut.io.clear, 0)

        stepUpdate(enqV=1, enq=1, deqR=u, clear=u, enqR=1, deqV=0, deq=x) // Step 1
        stepUpdate(     1,     2,      u,       u,      1,      0,     x) //      2
        stepUpdate(     1,     3,      1,       u,      1,      0,     x) //      3
        stepUpdate(     1,     4,      u,       u,      1,      1,     1) //      4
        stepUpdate(     1,     5,      u,       u,      1,      1,     2) //      5
        stepUpdate(     1,     6,      u,       u,      1,      1,     3) //      6
        stepUpdate(     1,     7,      u,       u,      1,      1,     4) //      7
      }
    } should be (true)
  }

  it should "operate while full" in {
    executeTest("fullOp", depth=4){
      dut : ClearableQueue[UInt] => new ClearableQueueTester(dut){

        poke(dut.io.enq.valid, 0)
        poke(dut.io.enq.bits, 0)
        poke(dut.io.deq.ready, 0)
        poke(dut.io.clear, 0)

        stepUpdate(enqV=1, enq=1, deqR=u, clear=u, enqR=1, deqV=0, deq=x) // Step 1
        stepUpdate(     1,     2,      u,       u,      1,      0,     x) //      2
        stepUpdate(     1,     3,      u,       u,      1,      0,     x) //      3
        stepUpdate(     1,     4,      1,       u,      1,      0,     x) //      4
        stepUpdate(     1,     5,      u,       u,      1,      1,     1) //      5
        stepUpdate(     1,     6,      u,       u,      1,      1,     2) //      6
        stepUpdate(     1,     7,      u,       u,      1,      1,     3) //      7
      }
    } should be (true)
  }

  it should "fill up" in {
    executeTest("fill", depth=4){
      dut : ClearableQueue[UInt] => new ClearableQueueTester(dut){

        poke(dut.io.enq.valid, 0)
        poke(dut.io.enq.bits, 0)
        poke(dut.io.deq.ready, 0)
        poke(dut.io.clear, 0)

        stepUpdate(enqV=1, enq=1, deqR=u, clear=u, enqR=1, deqV=0, deq=x) // Step 1
        stepUpdate(     1,     2,      u,       u,      1,      0,     x) //      2
        stepUpdate(     1,     3,      u,       u,      1,      0,     x) //      3
        stepUpdate(     1,     4,      u,       u,      1,      0,     x) //      4
        stepUpdate(     1,     5,      u,       u,      0,      0,     x) //      5
        stepUpdate(     1,     6,      u,       u,      0,      0,     x) //      6
        stepUpdate(     1,     7,      u,       u,      0,      0,     x) //      7
      }
    } should be (true)
  }

  it should "empty out" in {
    executeTest("empty", depth=4){
      dut : ClearableQueue[UInt] => new ClearableQueueTester(dut){

        poke(dut.io.enq.valid, 0)
        poke(dut.io.enq.bits, 0)
        poke(dut.io.deq.ready, 0)
        poke(dut.io.clear, 0)

        stepUpdate(enqV=1, enq=1, deqR=u, clear=u, enqR=1, deqV=0, deq=x) // Step 1
        stepUpdate(     1,     2,      u,       u,      1,      0,     x) //      2
        stepUpdate(     0,     3,      1,       u,      1,      0,     x) //      3
        stepUpdate(     u,     u,      u,       u,      1,      1,     1) //      4
        stepUpdate(     u,     u,      u,       u,      1,      1,     2) //      5
        stepUpdate(     u,     u,      u,       u,      1,      0,     x) //      6
        stepUpdate(     u,     u,      u,       u,      1,      0,     x) //      7
      }
    } should be (true)
  }

  it should "clear" in {
    executeTest("empty", depth=4){
      dut : ClearableQueue[UInt] => new ClearableQueueTester(dut){

        poke(dut.io.enq.valid, 0)
        poke(dut.io.enq.bits, 0)
        poke(dut.io.deq.ready, 0)
        poke(dut.io.clear, 0)

        stepUpdate(enqV=1, enq=1, deqR=u, clear=u, enqR=1, deqV=0, deq=x) // Step 1
        stepUpdate(     1,     2,      u,       u,      1,      0,     x) //      2
        stepUpdate(     0,     3,      1,       1,      1,      0,     x) //      3
        stepUpdate(     u,     u,      u,       0,      1,      0,     x) //      4
        stepUpdate(     u,     u,      u,       u,      1,      0,     x) //      5
        stepUpdate(     u,     u,      u,       u,      1,      0,     x) //      6
        stepUpdate(     u,     u,      u,       u,      1,      0,     x) //      7
      }
    } should be (true)
  }
}
