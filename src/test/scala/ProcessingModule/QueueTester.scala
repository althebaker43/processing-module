
package ProcessingModule

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, OrderedDecoupledHWIOTester, Driver, PeekPokeTester}

class QueueTester extends ChiselFlatSpec {

  behavior of "Queue"

  it should "dequeue correctly" in {
    assertTesterPasses {
      new OrderedDecoupledHWIOTester(){

        val device_under_test = Module(new QueueModule)
        inputEvent(device_under_test.io.in.bits -> 1)
        inputEvent(device_under_test.io.in.bits -> 2)
        inputEvent(device_under_test.io.in.bits -> 3)
        inputEvent(device_under_test.io.in.bits -> 4)

        outputEvent(device_under_test.io.out.bits -> 1)
        outputEvent(device_under_test.io.out.bits -> 2)
        outputEvent(device_under_test.io.out.bits -> 3)
        outputEvent(device_under_test.io.out.bits -> 4)
      }
    }
  }

  it should "dequeue with pulses" in {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/queue_pulse"), () => new QueueModule) {
      dut => new PeekPokeTester(dut){

        poke(dut.io.in.valid, 1)
        poke(dut.io.out.ready, 0)
        for (i <- 0 to 4) {
          poke(dut.io.in.bits, i)
          step(1)
        }

        poke(dut.io.in.valid, 0)
        poke(dut.io.out.ready, 1)
        step(1)
        expect(dut.io.out.bits, 1)

        poke(dut.io.out.ready, 0)
        step(1)
        expect(dut.io.out.bits, 1)

        poke(dut.io.out.ready, 1)
        step(1)
        expect(dut.io.out.bits, 2)

        poke(dut.io.out.ready, 0)
        step(1)
        expect(dut.io.out.bits, 2)
      }
    }
  }
}
