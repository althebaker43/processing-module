
package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.ChiselFlatSpec

class ProcessingModuleTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = AdderInstruction.width
  val queueDepth = 5

  behavior of "ProcessingModule"

  it should "initialize correctly" in {
    assertTesterPasses {
      new DecoupledTester("init"){

        val dut = Module(new AdderModule(dWidth))
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

        val dut = Module(new AdderModule(dWidth))

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

        val dut = Module(new AdderModule(dWidth))

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

        val dut = Module(new AdderModule(dWidth))

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

  ignore should "increment in the correct order" in {
    assertTesterPasses {
      new DecoupledTester("incrOrder") {

        val dut = Module(new AdderModule(dWidth))

        val events = new OutputEvent((dut.io.instr.pc, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc.bits, 1), (dut.io.data.out.value, 0)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeIncrData, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc.bits, 2)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc.bits, 3)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal=0.U))) ::
        new OutputEvent((dut.io.instr.pc.bits, 4)) ::
        new InputEvent((dut.io.instr.in, AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U))) ::
        new InputEvent((dut.io.data.in, 4)) ::
        new OutputEvent((dut.io.data.out.value, 4)) ::
        new OutputEvent((dut.io.data.out.value, 5)) ::
        Nil
      }
    }
  }

  it should "branch when greater than zero" in {
    assertTesterPasses{
      new DecoupledTester("bgt") {

        val dut = Module(new AdderModule(dWidth))

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

        val dut = Module(new AdderModule(dWidth))

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

        val dut = Module(new AdderModule(dWidth))

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
