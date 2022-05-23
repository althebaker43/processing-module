
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}
import chisel3.testers.TesterDriver

class FetchModuleTest extends ChiselFlatSpec {

  val iWidth = 4

  behavior of "FetchModule"

  it should "increment PC" in {
    assertTesterPasses {
      new DecoupledTester("incrPC"){
        val dut = Module(new FetchModule(iWidth))
        val events = new InputOutputEvent((dut.io.memInstr, 1)) ((dut.io.pcOut, 0), (dut.io.instr, 1)) ::
        new InputOutputEvent((dut.io.memInstr, 4)) ((dut.io.pcOut, 1), (dut.io.instr, 4)) ::
        Nil
      }
    }
  }

  it should "branch to absolute address" in {
    assertTesterPasses {
      new DecoupledTester("absBranch"){
        val dut = Module(new FetchModule(iWidth))
        val events = new InputOutputEvent((dut.io.memInstr, 1), (dut.io.branchPCIn, 4)) ((dut.io.pcOut, 0), (dut.io.instr, 1)) ::
        new InputOutputEvent((dut.io.memInstr, 11)) ((dut.io.pcOut, 1), (dut.io.instr, 11)) ::
        new InputOutputEvent((dut.io.memInstr, 2), (dut.io.branchPCIn, 4)) ((dut.io.pcOut, 4), (dut.io.instr, 2)) ::
        new InputOutputEvent((dut.io.memInstr, 6)) ((dut.io.pcOut, 5), (dut.io.instr, 6)) ::
        Nil
      }
    }
  }

  it should "branch relative to PC" in {
    assertTesterPasses {
      new DecoupledTester("relBranch"){
        val dut = Module(new FetchModule(iWidth))
        override def fixInputs : Unit = dut.io.relativeBranch := true.B
        val events = new InputOutputEvent((dut.io.memInstr, 1), (dut.io.branchPCIn, 4)) ((dut.io.pcOut, 0), (dut.io.instr, 1)) ::
        new InputOutputEvent((dut.io.memInstr, 6), (dut.io.branchPCIn, 4)) ((dut.io.pcOut, 4), (dut.io.instr, 6)) ::
        new InputOutputEvent((dut.io.memInstr, 11), (dut.io.branchPCIn, 4)) ((dut.io.pcOut, 8), (dut.io.instr, 11)) ::
        Nil
      }
    }
  }
}

class FetchModulePeekPokeTester extends ChiselFlatSpec {

  val iWidth = 4

  def executeTest(testName : String)(testerGen : FetchModule => PeekPokeTester[FetchModule]) : Boolean = Driver.execute(
    Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/fetch_" + testName), () => new FetchModule(4))(testerGen)

  behavior of "FetchModule"

  it should "increment PC" in {
    executeTest("incrPC"){
      dut : FetchModule => new PeekPokeTester(dut){

        poke(dut.io.instr.ready, 1)

        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.bits, 0)
        expect(dut.io.pcOut.valid, 1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits, 1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 4)
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits, 4)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 2)
      }
    }
  }
}
