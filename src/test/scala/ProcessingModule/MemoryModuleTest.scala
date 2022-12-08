
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}

class MemoryModuleTest extends ChiselFlatSpec {

  val instrs = new Instructions {
    def logic = 
      new InstructionLogic("incr") {
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        override def readMemory(instr : UInt) : Bool = false.B
        override def writeMemory(instr : UInt) : Bool = false.B
        override def writeRF(instr : UInt): Bool = true.B
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = instr(4,2)
        override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + 1.U
      } ::
    new InstructionLogic("add") {
      def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
      override def readMemory(instr : UInt) : Bool = false.B
      override def writeMemory(instr : UInt) : Bool = false.B
      override def writeRF(instr : UInt) : Bool = true.B
      override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = instr(4,2)
      override def getData(instr : UInt, pc : UInt, ops : Vec[UInt]) : UInt = ops(0) + ops(1)
    } ::
    new InstructionLogic("swap") {
      override val numOps : Int = 1
      override def decode(instr : UInt) : Bool = instr(1,0) === 3.U
      override def readMemory(instr : UInt) : Bool = true.B
      override def writeMemory(instr : UInt) : Bool = true.B
      override def writeRF(instr : UInt) : Bool = true.B
      override def getRFWriteData(resultData: UInt, memData: UInt): UInt = memData
    } ::
    Nil
  }

  def executeTest(testName : String)(testerGen : MemoryModule => PeekPokeTester[MemoryModule]) : Boolean = {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/mem_" + testName),
      () => new MemoryModule(dataWidth=4, addrWidth=8, rfDepth=8, instrs=instrs))(testerGen)
  }

  behavior of "MemoryModule"

  it should "write to RF" in {
    executeTest("writeRF") {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.results.readMem, false.B)
        poke(dut.io.results.writeMem, false.B)
        poke(dut.io.results.writeRF, false.B)
        poke(dut.io.results.instrValids(0), true.B)
        poke(dut.io.results.instrValids(1), false.B)
        poke(dut.io.results.instrValids(2), false.B)
        poke(dut.io.memDataOut.ready, true.B)
        poke(dut.io.memDataIn.valid, false.B)

        step(1)

        expect(dut.io.memDataOut.valid, false.B)
        expect(dut.io.memDataIn.ready, false.B)
        expect(dut.io.rfDataOut.valid, false.B)

        step(1)

        expect(dut.io.memDataOut.valid, false.B)
        expect(dut.io.memDataIn.ready, false.B)
        expect(dut.io.rfDataOut.valid, false.B)
      }
    } should be (true)
  }

  it should "swap memory with register" in {
    executeTest("swap") {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.results.addr, 5.U)
        poke(dut.io.results.rfIndex, 7.U)
        poke(dut.io.results.data, 1.U)
        poke(dut.io.results.readMem, true.B)
        poke(dut.io.results.writeMem, true.B)
        poke(dut.io.results.writeRF, true.B)
        poke(dut.io.results.instrValids(0), false.B)
        poke(dut.io.results.instrValids(1), false.B)
        poke(dut.io.results.instrValids(2), true.B)
        poke(dut.io.memDataOut.ready, true.B)
        poke(dut.io.memDataIn.valid, false.B)

        step(1)

        expect(dut.io.memAddr.valid, true.B)
        expect(dut.io.memAddr.bits, 5.U)
        expect(dut.io.resultsReady, false.B)
        expect(dut.io.memDataOut.valid, false.B)
        expect(dut.io.memDataIn.ready, true.B)
        expect(dut.io.rfDataOut.valid, false.B)

        poke(dut.io.memDataIn.valid, true.B)
        poke(dut.io.memDataIn.bits, 3.U)

        step(1)

        expect(dut.io.memAddr.valid, true.B)
        expect(dut.io.memAddr.bits, 5.U)
        expect(dut.io.resultsReady, true.B)
        expect(dut.io.memDataOut.valid, true.B)
        expect(dut.io.memDataOut.bits, 1.U)
        expect(dut.io.memDataIn.ready, false.B)
        expect(dut.io.rfDataOut.valid, true.B)
        expect(dut.io.rfDataOut.bits, 3.U)
      }
    } should be (true)
  }
}
