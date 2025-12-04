
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}

class DecodeTableTester(dut : DecodeFSMModule) extends TableTester(
  dut,
  List(
    dut.io.instrIn.valid,
    dut.io.instrIn.bits.pc,
    dut.io.instrIn.bits.word,
    dut.io.data.valid,
    dut.io.data.index,
    dut.io.data.preTrap,
    dut.io.data.word,
    dut.io.exData.valid,
    dut.io.exData.index,
    dut.io.exData.preTrap,
    dut.io.exData.word,
    dut.io.instrReady),
  List(
    dut.io.instrIn.ready,
    dut.io.branchPC.valid,
    dut.io.branchPC.bits,
    dut.io.instrValids(0),
    dut.io.instrValids(1),
    dut.io.instrValids(2),
    dut.io.instrValids(3),
    dut.io.instrOut.pc,
    dut.io.instrOut.word,
    dut.io.preTrapInstr))

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
        override def writeRF(instr : UInt) : Bool = true.B
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
    new InstructionLogic("ecall") {
      def decode ( instr : UInt ) : Bool = instr(1,0) === 0.U
      override def raiseException(instr : UInt, ops : Vec[UInt]) : Bool = true.B
    } ::
    Nil
  }

  val rfDepth : Int = 8

  def executeTest(testName : String, dbgMsg : Boolean = false)(testerGen : DecodeFSMModule => PeekPokeTester[DecodeFSMModule]) : Boolean = {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/decode_" + testName),
      () => new DecodeFSMModule(iWidth=8, pcWidth=6, instrs=instrs, numOps=2, opWidth=4, rfWidth=4, rfDepth=rfDepth, preTrapVector=20.U(6.W), dbgMsg=dbgMsg))(testerGen)
  }

  behavior of "DecodeModule"

  it should "decode add" in {
    executeTest("add") {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.data.preTrap, false.B)
        poke(dut.io.exData.valid, false.B)
        poke(dut.io.exData.preTrap, false.B)
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
        poke(dut.io.data.word, 1.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.index, 1.U)
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
        poke(dut.io.data.word, 4)
        poke(dut.io.data.index, 1)
        poke(dut.io.exData.valid, false.B)
        poke(dut.io.exData.preTrap, false.B)

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
    executeTest("branch", dbgMsg=true) {
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.instrIn.bits.word, "b000_010_11".U)
        poke(dut.io.instrIn.bits.pc, 0.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.word, 6.U)
        poke(dut.io.data.index, 2.U)
        poke(dut.io.exData.valid, false.B)

        step(rfDepth+1) // RF init
        poke(dut.io.instrIn.bits.word, "b000_000_01".U)
        poke(dut.io.instrIn.bits.pc, 1.U)
        poke(dut.io.data.valid, false.B)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 6.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_11".U)
        expect(dut.io.instrOut.pc, 0.U)
        expect(dut.io.branchPC.valid, true.B)
        expect(dut.io.branchPC.bits, 6.S)

        step(1)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 2.U)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)

        step(1)
        poke(dut.io.instrIn.bits.word, "b000_011_01".U)
        poke(dut.io.instrIn.bits.pc, 3.U)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)

        step(1)
        poke(dut.io.instrIn.bits.word, "b000_100_01".U)
        poke(dut.io.instrIn.bits.pc, 6.U)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)

        step(1)
        poke(dut.io.instrIn.bits.word, "b000_101_01".U)
        poke(dut.io.instrIn.bits.pc, 7.U)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_100_01".U)
        expect(dut.io.instrOut.pc, 6.U)
        expect(dut.io.branchPC.valid, false.B)

        step(1)
        poke(dut.io.instrIn.bits.word, "b000_110_01".U)
        poke(dut.io.instrIn.bits.pc, 8.U)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_101_01".U)
        expect(dut.io.instrOut.pc, 7.U)
        expect(dut.io.branchPC.valid, false.B)
      }
    } should be(true)
  }

  it should "stall on hazard" in {
    executeTest("stall"){
      dut => new PeekPokeTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, true.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.exData.valid, false.B)
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
        poke(dut.io.instrIn.bits.pc, 1.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        poke(dut.io.instrIn.bits.word, "b000_010_01".U)
        poke(dut.io.instrIn.bits.pc, 2.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        poke(dut.io.data.word, 1.U)
        poke(dut.io.data.valid, true.B)
        poke(dut.io.data.index, 1.U)
        poke(dut.io.instrIn.valid, false.B)

        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 1.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_001_01".U)
        expect(dut.io.instrOut.pc, 1.U)

        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_01".U)
        expect(dut.io.instrOut.pc, 2.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
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
        poke(dut.io.exData.valid, false.B)
        poke(dut.io.instrIn.bits.word, "b000_001_01".U)
        poke(dut.io.instrIn.bits.pc, 0.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)

        poke(dut.io.instrReady, true.B)

        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
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
        poke(dut.io.exData.valid, false.B)

        step(rfDepth) // RF init
        poke(dut.io.instrIn.bits.word, "b000_010_01".U)
        poke(dut.io.instrIn.bits.pc, 1.U)

        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_010_01".U)
        expect(dut.io.instrOut.pc, 1.U)
        poke(dut.io.instrReady, false.B)
        poke(dut.io.instrIn.bits.word, "b000_011_01".U)
        poke(dut.io.instrIn.bits.pc, 2.U)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.instrIn.ready, false.B)
        poke(dut.io.instrIn.valid, false.B)

        step(1)
        expect(dut.io.instrValids(0), false.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.instrIn.ready, false.B)
        poke(dut.io.instrReady, true.B)

        step(1)
        expect(dut.io.instrValids(0), true.B)
        expect(dut.io.instrValids(1), false.B)
        expect(dut.io.instrValids(2), false.B)
        expect(dut.io.ops(0), 0.U)
        expect(dut.io.ops(1), 0.U)
        expect(dut.io.instrOut.word, "b000_011_01".U)
        expect(dut.io.instrOut.pc, 2.U)
        expect(dut.io.instrIn.ready, true.B)
      }
    } should be(true)
  }

  it should "handle exceptions" in {
    executeTest("except"){
      dut => new DecodeTableTester(dut) {

        poke(dut.io.instrReady, true.B)
        poke(dut.io.instrIn.valid, false.B)
        poke(dut.io.data.valid, false.B)
        poke(dut.io.exData.valid, false.B)

        val i2 = 0x9 // incr 2, 000_010_01
        val ec = 0x0 // ecall

        step(rfDepth) // RF init

        //              instrIn     data            exData          ready   in  br                   instrOut
        //              V  pc   w   V   i   t   w   V   i   t   w   r       r   v   b   v0 v1 v2 v3  pc  w  pTr
        stepUpdate(List(0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, 0)) // Cycle 9
        stepUpdate(List(1,  u, i2,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, 0)) // Cycle 10
        stepUpdate(List(1,  1, ec,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  1, 0, 0, 0,  0, i2, 0)) // Cycle 11
        stepUpdate(List(0,  u,  u,  1,  2,  0,  1,  0,  0,  0,  0,  u,      1,  1, 20,  0, 0, 0, 0,  x,  x, x)) // Cycle 12
        stepUpdate(List(0,  u,  u,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, x)) // Cycle 12
        stepUpdate(List(1,  2, i2,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, x)) // Cycle 13
        stepUpdate(List(u,  3, i2,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, x)) // Cycle 14
        stepUpdate(List(u,  4, i2,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, x)) // Cycle 15
        stepUpdate(List(u, 20, i2,  0,  0,  0,  0,  0,  0,  0,  0,  u,      1,  0,  x,  0, 0, 0, 0,  x,  x, x)) // Cycle 16
        stepUpdate(List(u, 21, i2,  1,  2,  0,  2,  0,  0,  0,  0,  u,      1,  0,  x,  1, 0, 0, 0, 20, i2, 1)) // Cycle 17
      }
    } should be(true)
  }
}
