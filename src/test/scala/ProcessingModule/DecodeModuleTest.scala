
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class DecodeModuleTest extends ChiselFlatSpec {

  val instrs = new Instructions {
    def logic =
      new InstructionLogic("incr", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        override def getRFIndex ( instr : UInt, opIndex : Int ) : UInt = {
          opIndex match {
            case 0 => instr(4,2)
            case 1 => 0.U
          }
        }
        def execute ( instr : UInt ) : Unit = Unit
      } ::
    new InstructionLogic("add", dataInDepend=false, dataOutDepend=false) {
      def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
      override def getRFIndex ( instr : UInt, opIndex : Int ) : UInt = {
        opIndex match {
          case 0 => instr(4,2)
          case 1 => instr(7,5)
        }
      }
      def execute ( instr : UInt ) : Unit  = Unit
    } ::
    new InstructionLogic("beq", dataInDepend=false, dataOutDepend=false) {
      def decode ( instr : UInt ) : Bool = instr(1,0) === 3.U
      override def getRFIndex ( instr : UInt, opIndex : Int ) : UInt = {
        opIndex match {
          case 0 => instr(4,2)
          case 1 => instr
        }
      }
      override def branch : Bool = true.B
      override def getBranchPC( instr : UInt, ops : Vec[UInt] ) : UInt = {
        ops(0)
      }
      def execute ( instr : UInt ) : Unit  = Unit
    } ::
    Nil
  }

  behavior of "DecodeModule"

  it should "decode add" in {
    chisel3.iotesters.Driver(() => new DecodeModule(iWidth=8, instrs=instrs, numOps=2, opWidth=4, rfWidth=4, rfDepth=8)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)

        poke(dut.io.instrIn.bits, "b000_001_01".U)
        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut, "b000_001_01".U)

        poke(dut.io.instrIn.bits, "b011_001_10".U)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), true.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut, "b011_001_10".U)
      }
    } should be(true)
  }

  it should "write back" in {
    chisel3.iotesters.Driver(() => new DecodeModule(iWidth=8, instrs=instrs, numOps=2, opWidth=4, rfWidth=4, rfDepth=8)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.instrIn.valid, true.B)

        poke(dut.io.instrIn.bits, "b000_001_01".U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.bits, 4)
        poke(dut.io.index, 1)
        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 4.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut, "b000_001_01".U)
      }
    } should be(true)
  }

  it should "return branch PC" in {
    chisel3.iotesters.Driver(() => new DecodeModule(iWidth=8, instrs=instrs, numOps=2, opWidth=4, rfWidth=4, rfDepth=8)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.instrIn.valid, true.B)

        poke(dut.io.instrIn.bits, "b000_010_11".U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.bits, 12)
        poke(dut.io.index, 2)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), true.B)
        expect(dut.io.ops(0), 12.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut, "b000_010_11".U)
        expect(dut.io.branchPC.valid, true.B)
        expect(dut.io.branchPC.bits, 12.U)
      }
    } should be(true)
  }
}
