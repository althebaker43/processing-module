
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class FetchModuleTester extends ChiselFlatSpec {

  val iWidth = 4

  behavior of "FetchModule"

  it should "increments correctly" in {
    assertTesterPasses {
      new DecoupledTester("init"){
        val dut = Module(new FetchModule(iWidth))
        val events = new InputOutputEvent((dut.io.memInstr, 0)) ((dut.io.pcOut, 0), (dut.io.instr, 0)) ::
        new InputOutputEvent((dut.io.memInstr, 4)) ((dut.io.pcOut, 1), (dut.io.instr, 4)) ::
        Nil
      }
    }
  }
}
