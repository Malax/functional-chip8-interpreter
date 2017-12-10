package de.malax.chip8

package object opcode {
  type BinaryToOpcodeMapper = Int => Option[Opcode]
  type VariablesToOpcodeMapper = Map[Char, Int] => Option[Opcode]
}
