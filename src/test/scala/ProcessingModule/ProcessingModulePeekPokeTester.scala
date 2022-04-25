package ProcessingModule

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver}

class ProcessingModulePeekPokeTester extends ChiselFlatSpec {

  val dWidth = 4
  val iWidth = AdderInstruction.width
  val queueDepth = 5

  behavior of "AdderModule"

  ignore should "initialize correctly" in {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/adder_init"), () => new AdderModule(4)) {
      dut => new DecoupledPeekPokeTester(dut){
        def cycles = List(
          List(new InstrReq(addr = 0)),
          List(new InstrRcv(instr = AdderInstruction.createInt(AdderInstruction.codeStore, regVal=0.U, addrVal=3.U))),
          List(new StoreReq(addr = 3, data = 0)))
      }
    }
  }
}
