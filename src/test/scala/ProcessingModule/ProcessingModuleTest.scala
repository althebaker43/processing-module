
package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, OrderedDecoupledHWIOTester, HWIOTester, PeekPokeTester, Driver, IOAccessor}


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
  OrderedDecoupledHWIOTester.max_tick_count = 100
  // enable_all_debug = true
}

class Event(val portVals : Seq[(Data, BigInt)], val isInput : Boolean)
class InputEvent(portVals : (Data, BigInt)*) extends Event(portVals, true)
class OutputEvent(portVals : (Data, BigInt)*) extends Event(portVals, false)

abstract class DecoupledTester(val testerName : String) extends HWIOTester {

  override def desiredName() = testerName

  val max_tick_count = 100

  val dut : Module

  val device_under_test = dut

  val events : Seq[Event]

  override def finish() : Unit = {

    val eventCounter = RegInit(0.U(math.max(util.log2Ceil(events.size), 1).W))

    val eventFinished = Wire(Bool())
    eventFinished := true.B

    val tickCounter = RegInit(0.U(util.log2Ceil(max_tick_count).W))
    tickCounter := tickCounter + 1.U

    val ioAccessor = new IOAccessor(dut.io)

    for (input <- ioAccessor.dut_inputs) {
      input := 0.U
    }

    for ((event, i) <- events.zipWithIndex) {

      for ((port, value) <- event.portVals) {

        port match {

          case dPort : util.DecoupledIO[Data] => {
            when (eventCounter === i.U) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              if (event.isInput) {
                dPort.valid := true.B
                dPort.bits := value.U
                when (!dPort.ready) {
                  eventFinished := false.B
                }
              } else {
                dPort.ready := true.B
                when (dPort.valid) {
                  assert(dPort.bits.asUInt() === value.U, "Unexpected value: %d, expected: %d", dPort.bits.asUInt(), value.U)
                } .otherwise {
                  eventFinished := false.B
                }
              }
            }
          }

          case vPort : util.ValidIO[Data] => {
            when (eventCounter === i.U) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              if (event.isInput) {
                vPort.valid := true.B
                vPort.bits := value.U
              } else {
                when (vPort.valid) {
                  assert(vPort.bits.asUInt() === value.U, "Unexpected value: %d, expected: %d", vPort.bits.asUInt(), value.U)
                } .otherwise {
                  eventFinished := false.B
                }
              }
            }
          }

          case _ =>
        }
      }

      when (eventFinished) {
        eventCounter := eventCounter + 1.U
      }
    }

    when (eventCounter === events.size.U) {
      printf("All events completed!\n")
      stop()
    }
    when (tickCounter >= max_tick_count.U) {
      assert(false.B)
    }
  }
}

class ProcessingModuleTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = AdderInstruction.width
  val queueDepth = 5

  behavior of "ProcessingModule"

  it should "initialize correctly" in {
    assertTesterPasses {
      new DecoupledTester("init"){

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))
        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U, addrVal=3.U))) ::
        new OutputEvent((dut.io.data.out.value, 0), (dut.io.data.out.addr, 3)) ::
        Nil
      }
    }
  }

  it should "do nothing with a NOP instruction" in {
    assertTesterPasses {
      new DecoupledTester("nop"){

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeNOP, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc, 1)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))) ::
        new OutputEvent((dut.io.data.out.value, 0)) ::
        Nil
      }
    }
  }

  it should "increment by 1" in {
    assertTesterPasses {
      new DecoupledTester("incr1"){

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc, 1)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))) ::
        new OutputEvent((dut.io.data.out.value, 1)) ::
        Nil
      }
    }
  }

  it should "increment by a given value" in {
    assertTesterPasses {
      new DecoupledTester("incrVal"){

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeIncrData, regVal=0.U, addrVal=2.U))) ::
        new OutputEvent((dut.io.data.out.addr, 2)) ::
        new InputEvent((dut.io.data.in, 2)) ::
        new OutputEvent((dut.io.instr.pc, 1)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))) ::
        new OutputEvent((dut.io.data.out.value, 2)) ::
        Nil
      }
    }
  }

  // it should "increment in the correct order" in {
  //   assertTesterPasses {
  //     new NamedTester("incrOrder"){

  //       val device_under_test = Module(new AdderModule(dWidth, iWidth, queueDepth))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 0)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeNOP, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 1)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))
  //       outputEvent(device_under_test.io.data.out.value.bits -> 0)

  //       outputEvent(device_under_test.io.instr.pc.bits -> 2)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeNOP, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 3)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 4)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeIncrData, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 5)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 6)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 7)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))
  //       outputEvent(device_under_test.io.instr.pc.bits -> 8)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))
  //       inputEvent(device_under_test.io.data.in.bits -> 4)

  //       outputEvent(device_under_test.io.data.out.addr.bits -> 1)
  //       outputEvent(device_under_test.io.data.out.addr.bits -> 0)
  //       outputEvent(device_under_test.io.data.out.addr.bits -> 0)
  //       outputEvent(device_under_test.io.data.out.value.bits -> 0)
  //       outputEvent(device_under_test.io.data.out.value.bits -> 0)
  //       outputEvent(device_under_test.io.data.out.value.bits -> 4)
  //       outputEvent(device_under_test.io.data.out.value.bits -> 5)

  //       outputEvent(device_under_test.io.instr.pc.bits -> 9)
  //       inputEvent(device_under_test.io.instr.in.bits -> AdderInstruction.createInt(AdderInstruction.codeIncrData, regVal=0.U))
  //       outputEvent(device_under_test.io.data.out.addr.bits -> 1)
  //       inputEvent(device_under_test.io.data.in.bits -> 1)
  //     }
  //   }
  // }

  it should "branch when greater than zero" in {
    assertTesterPasses{
      new DecoupledTester("bgt") {

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc, 1)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeBGT, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc, 3)) ::
        Nil
      }
    }
  }

  it should "continue when equal to zero" in {
    assertTesterPasses {
      new DecoupledTester("bgt_eq") {

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeBGT, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc, 1)) ::
        Nil
      }
    }
  }

  it should "increment the correct register" in {
    assertTesterPasses{
      new DecoupledTester("incrReg") {

        val dut = Module(new AdderModule(dWidth, iWidth, queueDepth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc, 1)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=1.U))) ::
        new OutputEvent((dut.io.data.out.value, 0)) ::
        Nil
      }
    }
  }
}
