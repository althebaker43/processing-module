
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class MemoryModuleTest extends ChiselFlatSpec {

  behavior of "MemoryModule"

  it should "write to RF" in {
    chisel3.iotesters.Driver(() => new MemoryModule(dataWidth=4, addrWidth=8, rfDepth=8)){ dut =>
      new PeekPokeTester(dut) {

        poke(dut.io.results.readMem, false.B)
        poke(dut.io.results.writeMem, false.B)
        poke(dut.io.results.writeRF, false.B)
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
    }
  }
}
