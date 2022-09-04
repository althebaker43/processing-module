package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver}

class ProcessingModulePeekPokeTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = AdderInstruction.width

  def nop : BigInt = AdderInstruction.createInt(AdderInstruction.codeNOP, 0.U, 0.U)
  def incr1(regVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeIncr1, regVal.U, 0.U)
  def incrData(regVal : Int, addrVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeIncrData, regVal.U, addrVal.U)
  def store(regVal : Int, addrVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeStore, regVal.U, addrVal.U)
  def bgt(regVal : Int) : BigInt = AdderInstruction.createInt(AdderInstruction.codeBGT, regVal.U, 0.U)

  def executeTest(testName : String)(testerGen : AdderModule => DecoupledPeekPokeTester[AdderModule]) : Boolean = {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/adder_" + testName), () => new AdderModule(4))(testerGen)
  }

  behavior of "AdderModule"

  it should "initialize correctly" in {
    executeTest("init"){
      (dut : AdderModule) => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)), // fetch 0
          List(new InstrRcv(instr = store(0, 3))), // receive store
          List(new InstrReq(addr = 1)), // fetch 1, decode store
          List(new InstrRcv(instr = nop)), // receive nop, execute store
          List(new StoreReq(addr = 3, data = 0), new InstrReq(addr = 2))) // store to 3, fetch 2
      }
    } should be (true)
  }

  it should "process one nop per cycle" in {
    executeTest("nop"){
      (dut : AdderModule) => new DecoupledPeekPokeTester(dut) {
        def cycles = List(
          List(new InstrReq(addr = 0)),
          List(new InstrReq(addr = 1), new InstrRcv(instr = nop)),
          List(new InstrReq(addr = 2), new InstrRcv(instr = nop)),
          List(new InstrReq(addr = 3), new InstrRcv(instr = nop)),
          List(new InstrReq(addr = 4), new InstrRcv(instr = nop)),
          List(new InstrReq(addr = 5), new InstrRcv(instr = nop)),
          List(new InstrReq(addr = 6), new InstrRcv(instr = nop)),
          List(new InstrReq(addr = 7), new InstrRcv(instr = nop)))
      }
    } should be (true)
  }

  it should "increment by 1" in {
    executeTest("incr1"){
      (dut : AdderModule) => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)), // fetch 0
          List(new InstrRcv(instr = incr1(0)), new InstrReq(addr = 1)), // receive incr1, fetch 1
          Nil, // decode incr1
          List(new InstrRcv(instr = store(0, 6))), // receive store, execute incr1
          Nil, // decode store, memory incr1
          Nil, // execute store, writeback incr1
          List(new StoreReq(addr = 6, data = 1)), // store to 6
          List(new InstrReq(addr = 2)), // fetch 2
          List(new InstrRcv(instr = nop)), // receive nop
          List(new InstrReq(addr = 3)) // fetch 3
        )
      }
    } should be (true)
  }

  it should "process one incr1 per cycle" in {
    executeTest("incr1_cpi1"){
      (dut : AdderModule) => new DecoupledPeekPokeTester(dut) {
        def cycles = List(
          List(new InstrReq(addr = 0)),
          List(new InstrReq(addr = 1), new InstrRcv(instr = incr1(0))),
          List(new InstrReq(addr = 2), new InstrRcv(instr = incr1(1))),
          List(new InstrReq(addr = 3), new InstrRcv(instr = incr1(0))),
          List(new InstrReq(addr = 4), new InstrRcv(instr = incr1(1))),
          List(new InstrReq(addr = 5), new InstrRcv(instr = incr1(0))),
          List(new InstrReq(addr = 6), new InstrRcv(instr = incr1(1))),
          List(new InstrReq(addr = 7), new InstrRcv(instr = incr1(0))))
      }
    } should be (true)
  }

  it should "increment by 1 with a stall" in {
    executeTest("incr1stall"){
      (dut : AdderModule) => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)), // fetch 0
          List(new InstrRcv(instr = incr1(0)), new InstrReq(addr = 1)), // receive incr1, fetch 1
          List(new InstrRcv(instr = store(0, 6))), // receive store, decode incr1
          Nil, // stall store, execute incr1
          Nil, // decode store, memory incr1
          Nil, // execute store, writeback incr1
          List(new StoreReq(addr = 6, data = 1), new InstrReq(addr = 2)), // store to 6, fetch 2
          List(new InstrRcv(instr = nop)), // receive nop
          List(new InstrReq(addr = 3)) // fetch 3
        )
      }
    } should be (true)
  }

  it should "increment with memory value" in {
    executeTest("incrData"){
      (dut : AdderModule) => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)), // fetch 0
          List(new InstrRcv(instr = incrData(0, 1)), new InstrReq(addr = 1)), // receive incr1, fetch 1
          Nil, // decode incrData
          List(new InstrRcv(instr = store(0, 6))), // execute incrData, receive store
          List(new LoadRcv(4), new LoadReq(addr = 1)), // memory incrData, decode store
          Nil, // writeback incrData, execute store
          Nil, // memory store
          List(new StoreReq(addr = 6, data = 4)), // writeback store to 6
          Nil,
          Nil
        )
      }
    } should be (true)
  }
}
