
package ProcessingModule

import org.scalatest.{Matchers, FlatSpec}
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}

class DummyDecodeModule extends Module {

  val ops = Wire(Vec(2, UInt(4.W)))
  for (idx <- 0 until 2) {
    ops(idx) := 0.U
  }

  val regs = VecInit(Seq.fill(8){ RegInit(0.U(4.W)) })

  val instrs = new Instructions[Vec[UInt]] {
    def logic = 
      new InstructionLogic[Vec[UInt]]("incr", dataInDepend=false, dataOutDepend=false) {
        def decode ( instr : UInt ) : Bool =  instr(1,0) === 1.U
        def getOperands ( instr : UInt ) : Vec[UInt] = {
          val ops = Wire(Vec(2, UInt(4.W)))
          ops(0) := instr(4,2)
          ops(1) := 0.U
          ops
        }
        def execute ( instr : UInt ) : Unit = Unit
      } ::
  new InstructionLogic[Vec[UInt]]("add", dataInDepend=false, dataOutDepend=false) {
    def decode ( instr : UInt ) : Bool = instr(1,0) === 2.U
    def getOperands ( instr : UInt ) : Vec[UInt] = {
      val ops = Wire(Vec(2, UInt(4.W)))
      ops(0) := instr(4,2)
      ops(1) := instr(7,5)
      ops
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

  val decode = Module(new DecodeModule[Vec[UInt]](8, instrs, chiselTypeOf(ops)))

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
}

class DecodeModuleTest extends ChiselFlatSpec {

  behavior of "DecodeModule"

  it should "decode add" in {
    chisel3.iotesters.Driver(() => new DummyDecodeModule){ dut =>
      new DecodePeekPokeTester(dut)
    } should be(true)
  }
}
