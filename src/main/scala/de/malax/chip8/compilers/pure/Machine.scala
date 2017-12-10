package de.malax.chip8.compilers.pure

import de.malax.chip8._
import scodec.bits._

case class Machine private (pc: Int, display: BitVector, registers: Map[Register, Int], memory: ByteVector, inputState: Map[Key, Boolean], stack: List[Int], randomSeed: Long, powerState: Boolean) {
  def withRegisterValue(register: Register, value: Int): Machine = {
    copy(registers = registers.updated(register, value))
  }
}

object Machine {
  val initial = Machine(
    pc = 0x200,
    display = BitVector.fill(64 * 32)(high = false),
    registers = Register.all.map(r => r -> 0x00).toMap,
    memory = ByteVector.fill(0xFFF)(0x00).patch(0, Chip8Constants.memoryInterpreterArea),
    inputState = Key.all.map(k => k -> false).toMap,
    stack = Nil,
    randomSeed = 0xCAFEBABE,
    powerState = true
  )
}
