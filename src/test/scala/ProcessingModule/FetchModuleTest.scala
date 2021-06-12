
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import chisel3.testers.TesterDriver

class FetchModuleTest extends ChiselFlatSpec {

  val iWidth = 4

  behavior of "FetchModule"

  it should "increments correctly" in {
    assertTesterPasses {
      new DecoupledTester("incrPC"){
        val dut = Module(new FetchModule(iWidth))
        val events = new InputOutputEvent((dut.io.memInstr, 1)) ((dut.io.pcOut, 0), (dut.io.instr, 1)) ::
        new InputOutputEvent((dut.io.memInstr, 4)) ((dut.io.pcOut, 1), (dut.io.instr, 4)) ::
        Nil
      }
    }
  }

  it should "branches correctly" in {
    assertTesterPasses {
      new DecoupledTester("branchPC"){
        val dut = Module(new FetchModule(iWidth))
        val events = new InputOutputEvent((dut.io.memInstr, 1), (dut.io.branchPCIn, 4)) ((dut.io.pcOut, 0), (dut.io.instr, 1)) ::
        new InputOutputEvent((dut.io.memInstr, 6)) ((dut.io.pcOut, 4), (dut.io.instr, 6)) ::
        new InputOutputEvent((dut.io.memInstr, 11)) ((dut.io.pcOut, 5), (dut.io.instr, 11)) ::
        Nil
      }
    }
  }
}
