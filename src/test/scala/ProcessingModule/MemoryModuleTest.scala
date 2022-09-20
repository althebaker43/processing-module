
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class MemoryModuleTest extends ChiselFlatSpec {

  val instrs = new Instructions {
    def logic = 
      new InstructionLogic("incr", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        override def readMemory() : Bool = false.B
        override def writeMemory() : Bool = false.B
        override def writeRF(): Bool = true.B
        override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = instr(4,2)
        override def getData(instr : UInt, ops : Vec[UInt]) : UInt = ops(0) + 1.U
        def execute ( instr : UInt ) : Unit = Unit
      } ::
  new InstructionLogic("add", dataInDepend=false, dataOutDepend=false) {
    def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
    override def readMemory() : Bool = false.B
    override def writeMemory() : Bool = false.B
    override def writeRF() : Bool = true.B
    override def getWriteIndex(instr : UInt, ops : Vec[UInt]) : UInt = instr(4,2)
    override def getData(instr : UInt, ops : Vec[UInt]) : UInt = ops(0) + ops(1)
    def execute ( instr : UInt ) : Unit  = Unit
  } ::
    Nil
  }

  behavior of "MemoryModule"

  it should "write to RF" in {
    chisel3.iotesters.Driver(() => new MemoryModule(dataWidth=4, addrWidth=8, rfDepth=8, instrs=instrs)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.results.readMem, false.B)
        poke(dut.io.results.writeMem, false.B)
        poke(dut.io.results.writeRF, false.B)
        poke(dut.io.results.instrValids(0), true.B)
        poke(dut.io.results.instrValids(1), false.B)
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
}
