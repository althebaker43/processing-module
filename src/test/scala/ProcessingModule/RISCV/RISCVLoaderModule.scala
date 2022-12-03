package ProcessingModule.RISCV

import chisel3._
import scala.io.Source

object RISCVLoaderModule {

  def getHexNum(str : String) : Long = {
    val nums = for ((char, index) <- str.toUpperCase.reverse.zipWithIndex) yield {
      val num : Long = char match {
        case '0' => 0
        case '1' => 1
        case '2' => 2
        case '3' => 3
        case '4' => 4
        case '5' => 5
        case '6' => 6
        case '7' => 7
        case '8' => 8
        case '9' => 9
        case 'A' => 10
        case 'B' => 11
        case 'C' => 12
        case 'D' => 13
        case 'E' => 14
        case 'F' => 15
      }
      num << (index * 4)
    }
    nums.reduce(_ | _)
  }

  def getInstrArray(dumpPath : String) : Array[Long] = {

    val instrs = for {
      curLine <- Source.fromFile(dumpPath).getLines().toArray
      tokens = curLine.split("\t")
      if (tokens.length > 2)
      if (tokens(0).startsWith("8"))
      if (tokens(0).endsWith(":"))
        } yield {
      (getHexNum(tokens(0).stripPrefix("8").stripSuffix(":")),
        getHexNum(tokens(1).strip()))
    }

    println("Num instrs: " + instrs.length)
    for (pcInstr <- instrs) {
      println("Init: PC: " + pcInstr._1.toHexString + ", Instr: " + pcInstr._2.toHexString)
    }

    val vecIdxs = for (instr <- instrs) yield instr._1 >> 2
    val instrArr = new Array[Long]((vecIdxs.max + 1).toInt)
    for (pcInstr <- instrs) {
      instrArr((pcInstr._1 >> 2).toInt) = pcInstr._2
    }
    instrArr
  }
}

class RISCVLoaderModule(dumpPath : String) extends Module {

  val io = IO(new Bundle {
    val status = util.Valid(Bool())
  })

  io.status.valid := false.B
  io.status.bits := false.B

  val instrMem = VecInit(RISCVLoaderModule.getInstrArray(dumpPath).map(_.U(32.W)))

  val instr = Wire(util.Decoupled(UInt(32.W)))
  val pc = Wire(util.Valid(UInt(32.W)))

  val instrReg = Reg(util.Valid(UInt(32.W)))
  instr.valid := instrReg.valid
  instr.bits := instrReg.bits
  when (pc.valid & instr.ready) {
    instrReg.valid := true.B
    instrReg.bits := instrMem(pc.bits >> 2)
  } .otherwise {
    instrReg.valid := false.B
  }

  pc.valid := false.B
  instr.ready := false.B
  val dummyPC = RegInit(0.U(32.W))
  val prevDummyPC = RegNext(dummyPC)
  pc.bits := dummyPC
  when (dummyPC < 0x18c.U) {
    pc.valid := true.B
    instr.ready := true.B
  }
  when (instr.valid) {
    printf("Sim: PC: %x, Instr: %x\n", prevDummyPC, instr.bits)
    dummyPC := dummyPC + 4.U
  }
}
