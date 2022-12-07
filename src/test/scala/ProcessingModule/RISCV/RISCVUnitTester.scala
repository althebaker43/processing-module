package ProcessingModule.RISCV

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}

class RISCVUnitTester extends ChiselFlatSpec {

  def executeTest(testName : String)() : Boolean = {
    Driver.execute(Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/riscv_" + testName),
      () => new RISCVLoaderModule("src/test/resources/riscv/dump/" + testName + ".dump")){
      dut => new PeekPokeTester(dut) {
        while (peek(dut.io.ready) == BigInt(0)) {
          step(1)
        }
        var cycleCount = 0
        while ((peek(dut.io.status.valid) == BigInt(0)) && (cycleCount < 500)) {
          step(1)
          cycleCount += 1
        }
        expect(dut.io.status.valid, 1)
        expect(dut.io.status.bits, 1)
      }
    }
  }

  behavior of "RISCV"

  ignore should "initialize the loader" in {
    executeTest("rv32ui-p-simple") should be (true)
  }
}
