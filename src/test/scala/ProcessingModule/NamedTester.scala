
package ProcessingModule

import chisel3.iotesters.OrderedDecoupledHWIOTester

abstract class NamedTester(val testerName : String) extends OrderedDecoupledHWIOTester {
  override def desiredName() = testerName
  OrderedDecoupledHWIOTester.max_tick_count = 100
  // enable_all_debug = true
}
