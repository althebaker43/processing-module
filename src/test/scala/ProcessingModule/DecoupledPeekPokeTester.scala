package ProcessingModule

import chisel3._
import chisel3.iotesters.PeekPokeTester

abstract class ProcessingModuleEvent
class InstrReq(val addr : BigInt) extends ProcessingModuleEvent
class InstrRcv(val instr : BigInt) extends ProcessingModuleEvent
class StoreReq(val addr : BigInt, val data : BigInt) extends ProcessingModuleEvent
class LoadReq(val addr : BigInt) extends ProcessingModuleEvent
class LoadRcv(val data : BigInt) extends ProcessingModuleEvent

abstract class DecoupledPeekPokeTester[T <: ProcessingModule](dut : T) extends PeekPokeTester(dut) {

  def cycles : Seq[Seq[ProcessingModuleEvent]]

  for (cycle <- cycles) {

    println("tester: Starting new cycle")

    // Default input values
    poke(dut.io.instr.in.valid, 0)
    poke(dut.io.data.in.valid, 0)
    poke(dut.io.data.out.value.ready, 1)

    for (event <- cycle) {

      event match {

        case instrReq : InstrReq => {
          println("tester: Instruction request for address " + instrReq.addr)
          expect(dut.io.instr.pc.bits, instrReq.addr)
          expect(dut.io.instr.pc.valid, 1)
        }

        case instrRcv : InstrRcv => {
          println("tester: Receiving instruction " + instrRcv.instr)
          expect(dut.io.instr.in.ready, 1)
          poke(dut.io.instr.in.valid, 1)
          poke(dut.io.instr.in.bits, instrRcv.instr)
        }

        case storeReq : StoreReq => {
          println("tester: Store request for address " + storeReq.addr + " with data " + storeReq.data)
          expect(dut.io.data.out.addr.valid, 1)
          expect(dut.io.data.out.value.valid, 1)
          expect(dut.io.data.out.addr.bits, storeReq.addr)
          expect(dut.io.data.out.value.bits, storeReq.data)
        }

        case loadReq : LoadReq => {
          println("tester: Load request for address " + loadReq.addr)
          expect(dut.io.data.out.addr.valid, 1)
          expect(dut.io.data.out.addr.bits, loadReq.addr)
        }

        case loadRcv : LoadRcv => {
          println("tester: Receiving load data " + loadRcv.data)
          expect(dut.io.data.in.ready, 1)
          poke(dut.io.data.in.valid, 1)
          poke(dut.io.data.in.bits, loadRcv.data)
        }
      }
    }

    step(1)
  }
}
