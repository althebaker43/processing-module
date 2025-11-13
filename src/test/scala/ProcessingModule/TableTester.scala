package ProcessingModule

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver, Pokeable}

class SignalValue(val isValid : Boolean, val value : Int = 0)

abstract class TableTester[T <: MultiIOModule, U <: Element: Pokeable](dut : T, val inputs : Seq[U], val outputs : Seq[U]) extends PeekPokeTester[T](dut) {

  implicit def intToSignalValue(intVal : Int) : SignalValue = new SignalValue(isValid=true, value=intVal)

  val u = new SignalValue(isValid=false)
  val x = new SignalValue(isValid=false)

  def stepUpdate(values : Seq[SignalValue]) = {
    step(1)
    val valIter = values.iterator
    for (input <- inputs) {
      val value = valIter.next()
      if (value.isValid) {
        poke(input, value.value)
      }
    }
    for (output <- outputs) {
      val value = valIter.next()
      if (value.isValid) {
        expect(output, value.value)
      }
    }
  }
}
