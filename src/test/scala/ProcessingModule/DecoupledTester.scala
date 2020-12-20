
package ProcessingModule

import chisel3._
import chisel3.iotesters.{HWIOTester, IOAccessor}

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
