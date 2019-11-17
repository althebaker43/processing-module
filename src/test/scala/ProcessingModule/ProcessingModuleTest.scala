
package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, OrderedDecoupledHWIOTester, HWIOTester, PeekPokeTester, Driver}


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

abstract class NamedTester(val testerName : String) extends OrderedDecoupledHWIOTester {
  override def desiredName() = testerName
}

class ProcessingModuleTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = 3
  val queueDepth = 5

  behavior of "ProcessingModule"

  it should "initialize correctly" in {
    assertTesterPasses {
      new NamedTester("init"){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_DATA)
        inputEvent(device_under_test.io.data.in.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)
      }
    }
  }

  it should "do nothing with a NOP instruction" in {
    assertTesterPasses {
      new NamedTester("nop"){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_DATA)
        inputEvent(device_under_test.io.data.in.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)

        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)
      }
    }
  }

  it should "increment by 1" in {
    assertTesterPasses {
      new NamedTester("incr1"){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_DATA)
        inputEvent(device_under_test.io.data.in.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0, device_under_test.io.data.out.bits.memReq.bits -> 1)

        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_1)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 1)
      }
    }
  }

  it should "increment by a given value" in {
    assertTesterPasses {
      new NamedTester("incrVal"){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)

        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_DATA)
        outputEvent(device_under_test.io.data.out.bits.memReq.bits -> 1)

        inputEvent(device_under_test.io.data.in.bits -> 3)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 3)
      }
    }
  }

  it should "increment in the correct order" in {
    assertTesterPasses {
      new NamedTester("incrOrder"){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)

        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_DATA)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_1)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.data.in.bits -> 4)

        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.memReq.bits -> 1, device_under_test.io.data.out.bits.storeVal.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.memReq.bits -> 0, device_under_test.io.data.out.bits.storeVal.bits -> 4)
        outputEvent(device_under_test.io.data.out.bits.memReq.bits -> 0, device_under_test.io.data.out.bits.storeVal.bits -> 5)
      }
    }
  }
}
