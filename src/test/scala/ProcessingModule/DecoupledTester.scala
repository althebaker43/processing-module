
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

  def fixInputs() : Unit = {}

  override def finish() : Unit = {

    val eventCounter = RegInit(0.U(math.max(util.log2Ceil(events.size), 1).W + 1.W))
    val running = RegInit(false.B)
    when (!running) {
      running := true.B
    }

    val maxInputs = 64
    val maxOutputs = 64

    val inputsFinished = RegInit(VecInit(Seq.fill(maxInputs){ false.B }))
    val outputsFinished = RegInit(VecInit(Seq.fill(maxOutputs){ false.B }))
    val numInputsFinished = util.PopCount(inputsFinished)
    val numOutputsFinished = util.PopCount(outputsFinished)

    val tickCounter = RegInit(0.U(util.log2Ceil(max_tick_count).W))
    tickCounter := tickCounter + 1.U

    val ioAccessor = new IOAccessor(dut.io)

    for (input <- ioAccessor.dut_inputs) {
      input match {
        case x : SInt => input := 0.S
        case _ => input := 0.U
      }
    }

    fixInputs()

    for ((event, i) <- events.zipWithIndex) {

      for (((port, value), inputIdx) <- event.inputVals.zipWithIndex) {

        port match {

          case dPort : util.DecoupledIO[Data] => {
            when ((eventCounter === i.U) & running & !inputsFinished(inputIdx.U)) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              dPort.valid := true.B
              dPort.bits match {
                case x : SInt => dPort.bits := value.S
                case _ => dPort.bits := value.U
              }
              when (dPort.ready) {
                inputsFinished(inputIdx.U) := true.B
              }
            }
          }

          case vPort : util.ValidIO[Data] => {
            when ((eventCounter === i.U) & running) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              vPort.valid := true.B
              vPort.bits match {
                case x : SInt => vPort.bits := value.S
                case _ => vPort.bits := value.U
              }
              inputsFinished(inputIdx.U) := true.B
            }
          }

          case _ =>
        }
      }

      for (((port, value), outputIdx) <- event.outputVals.zipWithIndex) {

        port match {

          case dPort : util.DecoupledIO[Data] => {
            when ((eventCounter === i.U) & running & !outputsFinished(outputIdx.U)) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              dPort.ready := true.B
              when (dPort.valid) {
                assert(dPort.bits.asUInt() === value.U, "Unexpected value: %d, expected: %d", dPort.bits.asUInt(), value.U)
                outputsFinished(outputIdx.U) := true.B
              }
            }
          }

          case vPort : util.ValidIO[Data] => {
            when ((eventCounter === i.U) & running & !outputsFinished(outputIdx.U)) {
              printf("Waiting for event " + i + ": " + ioAccessor.port_to_name(port) + " = " + value + "\n")
              when (vPort.valid) {
                assert(vPort.bits.asUInt() === value.U, "Unexpected value: %d, expected: %d", vPort.bits.asUInt(), value.U)
                outputsFinished(outputIdx.U) := true.B
              }
            }
          }

          case _ =>
        }
      }

      when ((eventCounter === i.U) & running) {
        when ((numInputsFinished ===  event.inputVals.size.U) & (numOutputsFinished === event.outputVals.size.U) & running) {
          eventCounter := eventCounter + 1.U
          for (inputIdx <- 0 until maxInputs) {
            inputsFinished(inputIdx) := false.B
          }
          for (outputIdx <- 0 until maxOutputs) {
            outputsFinished(outputIdx) := false.B
          }
        } .otherwise {
          printf("Finished %d inputs and %d outputs\n", numInputsFinished, numOutputsFinished)
        }
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
