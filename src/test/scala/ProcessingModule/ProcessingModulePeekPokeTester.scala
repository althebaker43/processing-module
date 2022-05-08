package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver}

class ProcessingModulePeekPokeTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = AdderInstruction.width
  val queueDepth = 5

  def nop : BigInt = AdderInstruction.createInt(AdderInstruction.codeNOP, 0.U, 0.U)
  def incr1(regVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal.U, 0.U)
  def incrData(regVal : Int, addrVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeIncrData, regVal.U, addrVal.U)
  def store(regVal : Int, addrVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeStore, regVal.U, addrVal.U)
  def bgt(regVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeBGT, regVal.U, 0.U)

  def executeTest(testName : String)(testerGen : AdderModule => DecoupledPeekPokeTester[AdderModule]) : Boolean = Driver.execute(
    Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/adder_" + testName), () => new AdderModule(4))(testerGen)

  behavior of "AdderModule"

  it should "initialize correctly" in {
    executeTest("init"){
      dut : AdderModule => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)), // fetch 0
          List(new InstrRcv(instr = store(0, 3))), // receive store
          List(new InstrReq(addr = 1)), // fetch 1, decode store
          List(new InstrRcv(instr = nop)), // receive nop, execute store
          List(new StoreReq(addr = 3, data = 0), new InstrReq(addr = 2))) // store to 3, fetch 2
      }
    }
  }

  // TODO: resolve RAW hazard
  it should "increment by 1" in {
    executeTest("incr1"){
      dut : AdderModule => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)), // fetch 0
          List(new InstrRcv(instr = incr1(0))), // receive incr1
          List(new InstrReq(addr = 1)), // decode incr1, fetch 1
          Nil, // execute incr1
          Nil, // memory incr1
          List(new InstrRcv(instr = store(0, 6))), // receive store, writeback incr1
          List(new InstrReq(addr = 2)), // decode store, fetch 2
          List(new InstrRcv(instr = nop)), // execute store, receive nop
          List(new StoreReq(addr = 6, data = 1), new InstrReq(addr = 3)) // store to 6, fetch 3
        )
      }
    }
  }
}
