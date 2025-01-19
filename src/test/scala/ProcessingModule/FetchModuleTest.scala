
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}
import chisel3.testers.TesterDriver
import org.scalatest.Ignore

@Ignore
class FetchModuleTest extends ChiselFlatSpec {

  val iWidth = 4
  val pcWidth = 6
  val pcAlign = 1

  behavior of "FetchModule"

  it should "increment PC" in {
    assertTesterPasses {
      new DecoupledTester("incrPC"){
        val dut = Module(new FetchModule(iWidth, pcWidth, pcAlign))
        val events = new InputOutputEvent((dut.io.memInstr, 1)) ((dut.io.pcOut, 0), (dut.io.instr, 1)) ::
        new InputOutputEvent((dut.io.memInstr, 4)) ((dut.io.pcOut, 1), (dut.io.instr, 4)) ::
        Nil
      }
    }
  }

  it should "branch to absolute address" in {
    assertTesterPasses {
      new DecoupledTester("absBranch"){
        val dut = Module(new FetchModule(iWidth, pcWidth, pcAlign))
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
        val dut = Module(new FetchModule(iWidth, pcWidth, pcAlign))
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

  def executeTest(
    testName : String,
    pcWidth : Int = 6,
    pcAlign : Int = 1)(
    testerGen : FetchModule => PeekPokeTester[FetchModule]) : Boolean = Driver.execute(
    Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/fetch_" + testName), () => new FetchModule(iWidth, pcWidth, pcAlign))(testerGen)

  behavior of "FetchModule"

  it should "increment PC" in {
    executeTest("incrPC"){
      dut : FetchModule => new PeekPokeTester(dut){

        // Cycle 0, reset ends
        poke(dut.io.instr.ready, 1)

        // Cycle 1
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)

        // Cycle 2
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 0)

        // Cycle 3
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 1)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)

        // Cycle 4
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 1)
        expect(dut.io.instr.bits.pc, 0)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 2)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 5
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 3)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 4)

        // Cycle 6
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 4)
        expect(dut.io.instr.bits.pc, 1)
        expect(dut.io.pcOut.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 2)

        // Cycle 6
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 2)
        expect(dut.io.instr.bits.pc, 2)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 4)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 7)

        // Cycle 7
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 7)
        expect(dut.io.instr.bits.pc, 3)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 5)
        poke(dut.io.memInstr.valid, 0)
      }
    } should be (true)
  }

  it should "increment PC with alignment" in {
    executeTest("incrPCAlign", pcAlign=2){
      dut : FetchModule => new PeekPokeTester(dut){

        // Cycle 0, State init
        poke(dut.io.instr.ready, 1)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 1, State ready
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 0)

        // Cycle 2, State ready
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 0)

        // Cycle 3
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 2)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)

        // Cycle 4
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 4)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 1)
        expect(dut.io.instr.bits.pc, 0)
        expect(dut.io.memInstr.ready, 1)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 5
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 6)
        expect(dut.io.memInstr.ready, 1)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 4)

        // Cycle 6
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 4)
        expect(dut.io.instr.bits.pc, 2)
        expect(dut.io.pcOut.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        poke(dut.io.memInstr.bits, 7)

        // Cycle 7
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 8)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 7)
        expect(dut.io.instr.bits.pc, 4)
        expect(dut.io.memInstr.ready, 1)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 8
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 10)
      }
    } should be (true)
  }

  ignore should "branch to absolute address" in {
    executeTest("absBranch") {
      dut : FetchModule => new PeekPokeTester(dut) {

        poke(dut.io.instr.ready, 1)
        poke(dut.io.branchPCIn.valid, 0)
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.bits, 0)
        expect(dut.io.pcOut.valid, 1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)
        poke(dut.io.branchPCIn.valid, 1)
        poke(dut.io.branchPCIn.bits, 8)
        step(1)
        poke(dut.io.branchPCIn.valid, 0)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 8)
        poke(dut.io.memInstr.valid, 0)
        step(1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 5)
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 5)
        expect(dut.io.instr.bits.pc, 8)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 9)
        poke(dut.io.memInstr.valid, 0)
        step(1)
      }
    } should be (true)
  }

  ignore should "branch to relative address" in {
    executeTest("relBranch") {
      dut : FetchModule => new PeekPokeTester(dut) {

        poke(dut.io.instr.ready, 1)
        poke(dut.io.branchPCIn.valid, 0)
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.bits, 0)
        expect(dut.io.pcOut.valid, 1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 1)
        expect(dut.io.instr.bits.pc, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.bits, 1)
        expect(dut.io.pcOut.valid, 1)
        poke(dut.io.memInstr.valid, 0)
        step(1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 2)
        poke(dut.io.branchPCIn.valid, 1)
        poke(dut.io.branchPCIn.bits, 8)
        poke(dut.io.relativeBranch, 1)
        step(1)
        poke(dut.io.branchPCIn.valid, 0)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 9)
        poke(dut.io.memInstr.valid, 0)
        step(1)

        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 5)
        poke(dut.io.branchPCIn.valid, 0)
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 5)
        expect(dut.io.instr.bits.pc, 9)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 10)
        poke(dut.io.memInstr.valid, 0)
        step(1)
      }
    } should be (true)
  }

  it should "stall when output not ready" in {
    executeTest("stallOutput") {
      dut : FetchModule => new PeekPokeTester(dut) {

        poke(dut.io.instr.ready, 1)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 1, State ready
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 0)

        // Cycle 2, State ready
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 0)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)

        // Cycle 3, State buf
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)

        // Cycle 4
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 2)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 1)
        expect(dut.io.instr.bits.pc, 0)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 5
        step(1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 3)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 4)
        poke(dut.io.instr.ready, 0)

        // Cycle 6
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.pcOut.valid, 0)
        expect(dut.io.memInstr.ready, 0)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 7
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.pcOut.valid, 0)
        expect(dut.io.memInstr.ready, 0)

        // Cycle 8
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.pcOut.valid, 0)
        expect(dut.io.memInstr.ready, 0)
        poke(dut.io.instr.ready, 1)

        // Cycle 9
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 4)
        expect(dut.io.instr.bits.pc, 1)
        expect(dut.io.pcOut.valid, 0)
        expect(dut.io.memInstr.ready, 1)

         // Cycle 10
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 4)

        // Cycle 11
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 6)

        // Cycle 12
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 2)
        expect(dut.io.instr.bits.word, 6)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 0)
        poke(dut.io.memInstr.valid, 0)

        // Cycle 13
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pcOut.valid, 1)
        expect(dut.io.pcOut.bits, 5)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 8)
      }
    } should be (true)
  }
}

class PCGeneratorTester extends ChiselFlatSpec {

  val iWidth = 4

  def executeTest(
    testName : String,
    pcWidth : Int = 6,
    pcAlign : Int = 1)(
    testerGen : PCGenerator => PeekPokeTester[PCGenerator]) : Boolean = Driver.execute(
    Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/pcGen_" + testName), () => new PCGenerator(pcWidth, pcAlign))(testerGen)

  behavior of "PCGenerator"

  it should "increment PC" in {
    executeTest("incrPC"){
      dut : PCGenerator => new PeekPokeTester(dut){

        poke(dut.io.pc.ready, 1)

        step(1)

        poke(dut.io.pc.ready, 0)
        expect(dut.io.pc.valid, 1)
        expect(dut.io.pc.bits, 0)

        step(1)

        expect(dut.io.pc.valid, 0)

        step(1)

        expect(dut.io.pc.valid, 0)

        step(1)

        poke(dut.io.pc.ready, 1)
        expect(dut.io.pc.valid, 0)

        step(1)

        expect(dut.io.pc.valid, 1)
        expect(dut.io.pc.bits, 1)

        step(1)

        expect(dut.io.pc.valid, 1)
        expect(dut.io.pc.bits, 2)

        step(1)

        expect(dut.io.pc.valid, 1)
        expect(dut.io.pc.bits, 3)
        poke(dut.io.pc.ready, 0)

        step(1)

        expect(dut.io.pc.valid, 0)
        poke(dut.io.pc.ready, 1)

        step(1)

        expect(dut.io.pc.valid, 1)
        expect(dut.io.pc.bits, 4)
      }
    } should be (true)
  }
}

class PCBufferTester extends ChiselFlatSpec {

  val iWidth = 4

  def executeTest(
    testName : String,
    pcWidth : Int = 6,
    pcAlign : Int = 1)(
    testerGen : PCBuffer => PeekPokeTester[PCBuffer]) : Boolean = Driver.execute(
    Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/pcBuf_" + testName), () => new PCBuffer(iWidth, pcWidth, pcAlign))(testerGen)

  behavior of "PCBuffer"

  it should "align PC and instruction" in {
    executeTest("alignPC"){
      dut : PCBuffer => new PeekPokeTester(dut){

        // Cycle 0, State init
        poke(dut.io.instr.ready, 1)
        poke(dut.io.memInstr.valid, 0)
        poke(dut.io.pc.valid, 0)
        step(1)

        // Cycle 1, State ready
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.instr.ready, 0)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 0)
        step(1)

        // Cycle 2, State buf
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 10)
        poke(dut.io.pc.valid, 0)
        poke(dut.io.instr.ready, 1)
        step(1)

        // Cycle 3, State out
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 0)
        expect(dut.io.instr.bits.word, 10)
        poke(dut.io.memInstr.valid, 0)
        poke(dut.io.instr.ready, 0)
        step(1)

        // Cycle 4, State wait
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        step(1)

        // Cycle 5, State wait
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        step(1)

        // Cycle 6, State wait
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        step(1)

        // Cycle 7, State wait
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.instr.ready, 1)
        step(1)

        // Cycle 8, State wait
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 1)
        step(1)

        // Cycle 9, State wait
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 11)
        poke(dut.io.pc.bits, 2)
        step(1)

        // Cycle 10, State out
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 11)
        expect(dut.io.instr.bits.pc, 1)
        poke(dut.io.memInstr.bits, 12)
        poke(dut.io.pc.bits, 3)
        step(1)

        // Cycle 11, State out
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 12)
        expect(dut.io.instr.bits.pc, 2)
        poke(dut.io.memInstr.bits, 13)
        poke(dut.io.pc.bits, 3)
        step(1)

        // Cycle 12, State out
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.word, 13)
        expect(dut.io.instr.bits.pc, 3)
      }
    } should be (true)
  }

  it should "receive instructions immediately" in {
    executeTest("immInstr"){
      dut : PCBuffer => new PeekPokeTester(dut){

        // Cycle 0, State init
        poke(dut.io.instr.ready, 1)
        poke(dut.io.pc.valid, 0)
        poke(dut.io.pc.bits, 0)
        poke(dut.io.memInstr.valid, 0)
        step(1)

        // Cycle 1, State ready
        expect(dut.io.pc.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.pc.valid, 1)
        step(1)

        // Cycle 2, State buf
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.pc.bits, 1)
        step(1)

        // Cycle 3, State outBuf
        expect(dut.io.pc.ready, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.pc.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 1)
        step(1)

        // Cycle 3; State outBuf
        expect(dut.io.pc.ready, 1)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 0)
        expect(dut.io.instr.bits.word, 1)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 2)
        poke(dut.io.memInstr.valid, 0)
        step(1)

        // Cycle 4; State wait
        expect(dut.io.pc.ready, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.pc.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 4)
        step(1)

        // Cycle 5; State out
        expect(dut.io.pc.ready, 1)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 1)
        expect(dut.io.instr.bits.word, 4)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 3)
        poke(dut.io.memInstr.valid, 0)
        step(1)

        // Cycle 6
        expect(dut.io.pc.ready, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 0)
        poke(dut.io.pc.valid, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 7)
        step(1)

        // Cycle 7
        expect(dut.io.pc.ready, 1)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 2)
        expect(dut.io.instr.bits.word, 7)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 4)
      }
    } should be (true)
  }

  it should "wait for memory" in {
    executeTest("waitMem"){
      dut : PCBuffer => new PeekPokeTester(dut){

        // Cycle 0, end reset
        poke(dut.io.instr.ready, 1)
        poke(dut.io.memInstr.valid, 0)
        poke(dut.io.pc.valid, 0)

        // Cycle 1
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)

        // Cycle 2
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 0)

        // Cycle 3
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 10)
        poke(dut.io.pc.bits, 1)

        // Cycle 4
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 0)
        expect(dut.io.instr.bits.word, 10)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.memInstr.bits, 11)
        poke(dut.io.pc.bits, 2)

        // Cycle 5
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 1)
        expect(dut.io.instr.bits.word, 11)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.memInstr.valid, 0)
        poke(dut.io.pc.bits, 3)

        // Cycle 6
        step(1)
        expect(dut.io.instr.valid, 0)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 0)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 12)
        poke(dut.io.pc.bits, 4)

        // Cycle 7
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 2)
        expect(dut.io.instr.bits.word, 12)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.memInstr.valid, 1)
        poke(dut.io.memInstr.bits, 13)
        poke(dut.io.pc.valid, 0)

        // Cycle 8
        step(1)
        expect(dut.io.instr.valid, 1)
        expect(dut.io.instr.bits.pc, 3)
        expect(dut.io.instr.bits.word, 13)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.memInstr.bits, 14)
        poke(dut.io.pc.valid, 1)
        poke(dut.io.pc.bits, 5)

        // Cycle 9
        step(1)
        expect(dut.io.instr.bits.pc, 4)
        expect(dut.io.instr.bits.word, 14)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.pc.bits, 6)
        poke(dut.io.memInstr.bits, 15)

        // Cycle 10
        step(1)
        expect(dut.io.instr.bits.pc, 5)
        expect(dut.io.instr.bits.word, 15)
        expect(dut.io.memInstr.ready, 1)
        expect(dut.io.pc.ready, 1)
        poke(dut.io.pc.bits, 7)
        poke(dut.io.memInstr.bits, 0)
      }
    } should be (true)
  }
}
