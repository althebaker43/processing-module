
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class DummyDecodeModule extends Module {

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
    Nil
  }

  val io = IO(new Bundle {
    val instr = Flipped(util.Valid(UInt(8.W)))
    val instrValids = Output(Vec(2, Bool()))
    val ops = Output(Vec(2, UInt(4.W)))
  })

  val decode = Module(new DecodeModule(
    iWidth=8,
    instrs=instrs,
    numOps=2,
    opWidth=4,
    rfWidth=4,
    rfDepth=8))

  io <> decode.io
}

class DecodePeekPokeTester(dut : DummyDecodeModule) extends PeekPokeTester(dut) {

  poke(dut.io.instr.valid, true.B)

  poke(dut.io.instr.bits, "b000_001_01".U)
  step(1)
  expect(dut.io.instrValids(0), true.B)
  expect(dut.io.instrValids(1), false.B)
  expect(dut.io.ops(0), 0.U)
  expect(dut.io.ops(1), 0.U)

  poke(dut.io.instr.bits, "b011_001_10".U)
  step(1)
  expect(dut.io.instrValids(0), false.B)
  expect(dut.io.instrValids(1), true.B)
  expect(dut.io.ops(0), 0.U)
  expect(dut.io.ops(1), 0.U)
}

class DecodeModuleTest extends ChiselFlatSpec {

  behavior of "DecodeModule"

  it should "decode add" in {
    chisel3.iotesters.Driver(() => new DummyDecodeModule){ dut =>
      new DecodePeekPokeTester(dut)
    } should be(true)
  }
}
