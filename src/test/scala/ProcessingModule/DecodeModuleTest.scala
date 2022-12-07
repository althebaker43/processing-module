
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}

class DecodeModuleTest extends ChiselFlatSpec {

  val instrs = new Instructions {
    def logic =
      new InstructionLogic("incr") {
        override val numOps = 1
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        override def getRFIndex ( instr : UInt, opIndex : Int ) : UInt = {
          opIndex match {
            case 0 => instr(4,2)
            case 1 => 0.U
          }
        }
        override def writeRF() : Bool = true.B
        override def getWriteIndex( instr : UInt, ops : Vec[UInt] ) : UInt = instr(4,2)
      } ::
    new InstructionLogic("add") {
      override val numOps = 2
      def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
      override def getRFIndex ( instr : UInt, opIndex : Int ) : UInt = {
        opIndex match {
          case 0 => instr(4,2)
          case 1 => instr(7,5)
        }
      }
    } ::
    new InstructionLogic("beq") {
      override val numOps = 1
      def decode ( instr : UInt ) : Bool = instr(1,0) === 3.U
      override def getRFIndex ( instr : UInt, opIndex : Int ) : UInt = {
        opIndex match {
          case 0 => instr(4,2)
          case 1 => instr
        }
      }
      override def branch : Bool = true.B
      override def getBranchPC( instr : UInt, ops : Vec[UInt] ) : SInt = {
        ops(0).asSInt
      }
    } ::
    Nil
  }

  val rfDepth : Int = 8

  def executeTest(testName : String)(testerGen : DecodeModule => PeekPokeTester[DecodeModule]) : Boolean = {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/decode_" + testName),
      () => new DecodeModule(iWidth=8, pcWidth=6, instrs=instrs, numOps=2, opWidth=4, rfWidth=4, rfDepth=rfDepth))(testerGen)
  }

  behavior of "DecodeModule"

  it should "decode add" in {
    executeTest("add") {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 0.U)

        step(rfDepth+1) // RF init
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)

        poke(dut.io.instrIn.bits.word, "b011_001_10".U)
        poke(dut.io.instrIn.bits.pc, 1.U)
        poke(dut.io.data.bits, 1.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.index, 1.U)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), true.B)
        expect(dut.io.ops(0), 1.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b011_001_10".U)
        expect(dut.io.instrOut.pc, 1.U)
      }
    } should be(true)
  }

  it should "write back" in {
    executeTest("write_back") {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 0.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.bits, 4)
        poke(dut.io.index, 1)

        step(rfDepth+1) // RF init
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 4.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)
      }
    } should be(true)
  }

  it should "return branch PC" in {
    executeTest("branch") {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.instrIn.bits.word, "b000_010_11".U)
        poke(dut.io.instrIn.bits.pc, 0.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.bits, 6)
        poke(dut.io.index, 2)

        step(rfDepth+1) // RF init
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), true.B)
        expect(dut.io.ops(0), 6.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_11".U)
        expect(dut.io.instrOut.pc, 0.U)
        expect(dut.io.branchPC.valid, true.B)
        expect(dut.io.branchPC.bits, 6.S)
      }
    } should be(true)
  }

  it should "stall on hazard" in {
    executeTest("stall"){
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 0.U)

        step(rfDepth+1) // RF init
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)

        poke(dut.io.data.bits, 1.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.index, 1.U)
        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 1.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)
      }
    } should be(true)
  }

  it should "stall on output not ready" in {
    executeTest("stallOutput"){
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, false.B)

        step(rfDepth) // RF init

        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 0.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, 0.U)
        expect(dut.io.instrOut.pc, 0.U)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, 0.U)
        expect(dut.io.instrOut.pc, 0.U)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, 0.U)
        expect(dut.io.instrOut.pc, 0.U)

        poke(dut.io.instrReady, true.B)
        step(2)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)
      }
    } should be(true)
  }

  it should "stall on output not ready when instr arrives" in {
    executeTest("stallOutputInstr"){
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 0.U)

        step(rfDepth+1) // RF init
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 0.U)

        poke(dut.io.instrReady, false.B)
        poke(dut.io.instrIn.bits.word, "b000_010_01".U)
        poke(dut.io.instrIn.bits.pc, 1.U)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_01".U)
        expect(dut.io.instrOut.pc, 1.U)
        expect(dut.io.instrIn.ready, false.B)

        poke(dut.io.instrIn.valid, false.B)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_01".U)
        expect(dut.io.instrOut.pc, 1.U)
        expect(dut.io.instrIn.ready, false.B)

        poke(dut.io.instrReady, true.B)
        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_01".U)
        expect(dut.io.instrOut.pc, 1.U)
        expect(dut.io.instrIn.ready, true.B)
      }
    } should be(true)
  }
}
