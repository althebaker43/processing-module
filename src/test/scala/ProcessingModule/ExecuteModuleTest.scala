
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class ExecuteModuleTest extends ChiselFlatSpec {

  val instrs = new Instructions {
    def logic = 
      new InstructionLogic("incr", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        override def readMemory() : Bool = false.B
        override def writeMemory() : Bool = false.B
        override def writeRF(): Bool = true.B
        override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = instr(4,2)
        override def getData(instr : UInt, ops : Vec[UInt]) : UInt = ops(0) + 1.U
        def execute ( instr : UInt ) : Unit = Unit
      } ::
  new InstructionLogic("add", dataInDepend=false, dataOutDepend=false) {
    def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
    override def readMemory() : Bool = false.B
    override def writeMemory() : Bool = false.B
    override def writeRF() : Bool = true.B
    override def getAddress(instr : UInt, ops : Vec[UInt]) : UInt = instr(4,2)
    override def getData(instr : UInt, ops : Vec[UInt]) : UInt = ops(0) + ops(1)
    def execute ( instr : UInt ) : Unit  = Unit
  } ::
    Nil
  }

  behavior of "ExecuteModule"

  it should "execute incr" in {
    chisel3.iotesters.Driver(() => new ExecuteModule(iWidth=8, instrs=instrs, numOps=2, opWidth=4, dataWidth=4, addrWidth=8)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.instr, "b000_000_01".U)
        poke(dut.io.instrValids(0), true.B)
        poke(dut.io.instrValids(1), false.B)
        poke(dut.io.ops(0), 0.U)
        poke(dut.io.ops(1), 0.U)

        step(1)

        expect(dut.io.results.addr, 0.U)
        expect(dut.io.results.data, 1.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)
      }
    }
  }

  it should "execute add" in {
    chisel3.iotesters.Driver(() => new ExecuteModule(iWidth=8, instrs=instrs, numOps=2, opWidth=4, dataWidth=4, addrWidth=8)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.instr, "b011_001_10".U)
        poke(dut.io.instrValids(0), false.B)
        poke(dut.io.instrValids(1), true.B)
        poke(dut.io.ops(0), 1.U)
        poke(dut.io.ops(1), 2.U)

        step(1)

        expect(dut.io.results.addr, 1.U)
        expect(dut.io.results.data, 3.U)
        expect(dut.io.results.readMem, false.B)
        expect(dut.io.results.writeMem, false.B)
        expect(dut.io.results.writeRF, true.B)
      }
    }
  }
}
