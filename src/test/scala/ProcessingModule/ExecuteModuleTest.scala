
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}

class ExecuteModuleTest extends ChiselFlatSpec {

  def getInstrReg(instr : UInt) : UInt = instr(4,2)

  val instrs = new Instructions {
    def logic = 
      new InstructionLogic("incr") {
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        override def readMemory() : Bool = false.B
        override def writeMemory() : Bool = false.B
        override def writeRF(): Bool = true.B
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getInstrReg(instr)
        override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + 1.U
      } ::
    new InstructionLogic("add") {
      def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
      override def readMemory() : Bool = false.B
      override def writeMemory() : Bool = false.B
      override def writeRF() : Bool = true.B
      override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getInstrReg(instr)
      override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + ops(1)
    } ::
    new InstructionLogic("addpc") {
      def decode ( instr : UInt ) : Bool = instr(1,0) === 3.U
      override def readMemory() : Bool = false.B
      override def writeMemory() : Bool = false.B
      override def writeRF() : Bool = true.B
      override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = getInstrReg(instr)
      override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + pc
    } ::
    Nil
  }

  def executeTest(testName : String)(testerGen : ExecuteModule => PeekPokeTester[ExecuteModule]) : Boolean = {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/exec_" + testName),
      () => new ExecuteModule(iWidth=8, pcWidth=6, instrs=instrs, numOps=2, opWidth=4, dataWidth=4, addrWidth=8, rfDepth=8))(testerGen)
  }

  behavior of "ExecuteModule"

  it should "execute incr" in {
    executeTest("incr") {
      dut => new PeekPokeTester(dut){

        poke(dut.io.instr.word, "b000_000_01".U)
        poke(dut.io.instr.pc, 0.U)
        poke(dut.io.instrValids(0), true.B)
        poke(dut.io.instrValids(1), false.B)
        poke(dut.io.instrValids(2), false.B)
        poke(dut.io.ops(0), 0.U)
        poke(dut.io.ops(1), 0.U)
        step(1)
        expect(dut.io.results.rfIndex, 0.U)
        expect(dut.io.results.data, 1.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)

        poke(dut.io.instr.word, "b000_001_01".U)
        poke(dut.io.instr.pc, 1.U)
        poke(dut.io.instrValids(0), true.B)
        poke(dut.io.instrValids(1), false.B)
        poke(dut.io.instrValids(2), false.B)
        poke(dut.io.ops(0), 0.U)
        poke(dut.io.ops(1), 0.U)
        step(1)
        expect(dut.io.results.rfIndex, 1.U)
        expect(dut.io.results.data, 1.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)
      }
    } should be (true)
  }

  it should "execute add" in {
    executeTest("add") {
      dut => new PeekPokeTester(dut){

        poke(dut.io.instr.word, "b011_001_10".U)
        poke(dut.io.instr.pc, 0.U)
        poke(dut.io.instrValids(0), false.B)
        poke(dut.io.instrValids(1), true.B)
        poke(dut.io.instrValids(2), false.B)
        poke(dut.io.ops(0), 1.U)
        poke(dut.io.ops(1), 2.U)
        step(1)
        expect(dut.io.results.rfIndex, 1.U)
        expect(dut.io.results.data, 3.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)

        poke(dut.io.instr.word, "b011_000_10".U)
        poke(dut.io.instr.pc, 1.U)
        poke(dut.io.instrValids(0), false.B)
        poke(dut.io.instrValids(1), true.B)
        poke(dut.io.instrValids(2), false.B)
        poke(dut.io.ops(0), 1.U)
        poke(dut.io.ops(1), 2.U)
        step(1)
        expect(dut.io.results.rfIndex, 0.U)
        expect(dut.io.results.data, 3.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)
      }
    } should be (true)
  }

  it should "execute addpc" in {
    executeTest("addpc") {
      dut => new PeekPokeTester(dut){

        poke(dut.io.instr.word, "b011_001_11".U)
        poke(dut.io.instr.pc, 0.U)
        poke(dut.io.instrValids(0), false.B)
        poke(dut.io.instrValids(1), false.B)
        poke(dut.io.instrValids(2), true.B)
        poke(dut.io.ops(0), 1.U)
        poke(dut.io.ops(1), 2.U)
        step(1)
        expect(dut.io.results.rfIndex, 1.U)
        expect(dut.io.results.data, 1.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)

        poke(dut.io.instr.word, "b011_000_11".U)
        poke(dut.io.instr.pc, 1.U)
        poke(dut.io.instrValids(0), false.B)
        poke(dut.io.instrValids(1), false.B)
        poke(dut.io.instrValids(2), true.B)
        poke(dut.io.ops(0), 3.U)
        poke(dut.io.ops(1), 2.U)
        step(1)
        expect(dut.io.results.rfIndex, 0.U)
        expect(dut.io.results.data, 4.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)
      }
    } should be (true)
  }
}
