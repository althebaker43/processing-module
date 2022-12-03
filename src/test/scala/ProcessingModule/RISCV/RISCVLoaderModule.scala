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

  val memDataOut = Wire(util.Decoupled(UInt(32.W)))
  val memDataInAddr = Wire(util.Valid(UInt(32.W)))
  val memDataIn = Wire(util.Decoupled(UInt(32.W)))
  memDataOut.valid := true.B
  memDataOut.bits := 0.U
  memDataIn.ready := true.B

  val dut = Module(new RISCVProcessingModule(rfDepth = 4096 + 32))
  dut.io.instr.in <> instr
  dut.io.instr.pc <> pc
  dut.io.data.in <> memDataOut
  dut.io.data.out.value <> memDataIn
  dut.io.data.out.addr <> memDataInAddr

  val prevPC = RegNext(pc.bits)
  when (instr.valid) {
    printf("Sim: PC: %x, Instr: %x\n", prevPC, instr.bits)
    // when (instr.bits === 0x73.U) {
    //   io.status.valid := true.B
    // }
  }

  when (memDataInAddr.valid &
    (memDataInAddr.bits === 0.U) &
    memDataIn.valid) {

    printf("Sim: Addr: %x, Data: %x\n", memDataInAddr.bits, memDataIn.bits)

    when (memDataIn.bits === 0.U) {
      io.status.bits := false.B
    } .otherwise {
      io.status.bits := true.B
    }

    io.status.valid := true.B
  }
}
