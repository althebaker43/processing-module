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
}

class RISCVLoaderModule(dumpPath : String) extends Module {

  val io = IO(new Bundle {
    val status = util.Valid(Bool())
  })

  io.status.valid := false.B
  io.status.bits := false.B

  val instrs = for {
    curLine <- Source.fromFile(dumpPath).getLines().toArray
    tokens = curLine.split("\t")
    if (tokens.length > 2)
    if (tokens(0).startsWith("8"))
    if (tokens(0).endsWith(":"))
      } yield {
    (RISCVLoaderModule.getHexNum(tokens(0).stripPrefix("8").stripSuffix(":")),
      RISCVLoaderModule.getHexNum(tokens(1).strip()))
  }

  println("Num instrs: " + instrs.length)

  for (pcInstr <- instrs) {
    println("PC: " + pcInstr._1.toHexString + ", Instr: " + pcInstr._2.toHexString)
  }
}
