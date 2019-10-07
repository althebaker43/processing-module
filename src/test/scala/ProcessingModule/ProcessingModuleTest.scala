
package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, OrderedDecoupledHWIOTester}


class ProcessingModuleTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = 3
  val queueDepth = 5

  behavior of "ProcessingModule"

  it should "initialize correctly" in {
    assertTesterPasses {
      new OrderedDecoupledHWIOTester(){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.data.in.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)
      }
    }
  }

  it should "do nothing with a NOP instruction" in {
    assertTesterPasses {
      new OrderedDecoupledHWIOTester(){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.data.in.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)

        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)
      }
    }
  }

  it should "increment by 1" in {
    assertTesterPasses{
      new OrderedDecoupledHWIOTester(){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.data.in.bits -> 0)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 0)

        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_INCR_1)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        outputEvent(device_under_test.io.data.out.bits.storeVal.bits -> 1)
      }
    }
  }

  it should "increment by a given value" in {
    assertTesterPasses{
      new OrderedDecoupledHWIOTester(){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.data.in.bits -> 0)
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
    assertTesterPasses{
      new OrderedDecoupledHWIOTester(){

        val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_NOP)
        inputEvent(device_under_test.io.instr.bits -> AdderModule.INSTR_STORE)
        inputEvent(device_under_test.io.data.in.bits -> 0)
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
