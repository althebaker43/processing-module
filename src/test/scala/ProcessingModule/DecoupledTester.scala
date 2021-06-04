
package ProcessingModule

import chisel3._
import chisel3.iotesters.{HWIOTester, IOAccessor}

class Event(val inputVals : Seq[(Data, BigInt)], val outputVals : Seq[(Data, BigInt)])
class InputEvent(portVals : (Data, BigInt)*) extends Event(inputVals=portVals, outputVals=Nil)
class OutputEvent(portVals : (Data, BigInt)*) extends Event(inputVals=Nil, outputVals=portVals)
class InputOutputEvent(inputs : (Data, BigInt)*)(outputs : (Data, BigInt)*) extends Event(inputVals=inputs, outputVals=outputs)

abstract class DecoupledTester(val testerName : String) extends HWIOTester {

  override def desiredName() = testerName

  val max_tick_count = 100

  val dut : Module

  val device_under_test = dut

  val events : Seq[Event]

  override def finish() : Unit = {

    val eventCounter = RegInit(0.U(math.max(util.log2Ceil(events.size), 1).W + 1.W))

    val eventFinished = Wire(Bool())
    eventFinished := true.B

    val tickCounter = RegInit(0.U(util.log2Ceil(max_tick_count).W))
    tickCounter := tickCounter + 1.U

    val ioAccessor = new IOAccessor(dut.io)

    for (input <- ioAccessor.dut_inputs) {
      input := 0.U
    }

    for ((event, i) <- events.zipWithIndex) {

      for ((port, value) <- event.inputVals) {

        port match {

          case dPort : util.DecoupledIO[Data] => {
            when (eventCounter === i.U) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              dPort.valid := true.B
              dPort.bits := value.U
              when (!dPort.ready) {
                eventFinished := false.B
              }
            }
          }

          case vPort : util.ValidIO[Data] => {
            when (eventCounter === i.U) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              vPort.valid := true.B
              vPort.bits := value.U
            }
          }

          case _ =>
        }
      }

      for ((port, value) <- event.outputVals) {

        port match {

          case dPort : util.DecoupledIO[Data] => {
            when (eventCounter === i.U) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              dPort.ready := true.B
              when (dPort.valid) {
                assert(dPort.bits.asUInt() === value.U, "Unexpected value: %d, expected: %d", dPort.bits.asUInt(), value.U)
              } .otherwise {
                eventFinished := false.B
              }
            }
          }

          case vPort : util.ValidIO[Data] => {
            when (eventCounter === i.U) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              when (vPort.valid) {
                assert(vPort.bits.asUInt() === value.U, "Unexpected value: %d, expected: %d", vPort.bits.asUInt(), value.U)
              } .otherwise {
                eventFinished := false.B
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
