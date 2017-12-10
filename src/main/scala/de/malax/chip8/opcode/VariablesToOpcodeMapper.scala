package de.malax.chip8.opcode

import de.malax.chip8.Register

object VariablesToOpcodeMapper {
  def valueOpcode(f: Int => Opcode, valueVariable: Char = 'n'): VariablesToOpcodeMapper = variables => Some(f(variables('n')))

  def registerValueOpcode(f: (Register, Int) => Opcode, registerVariable: Char = 'x', valueVariable: Char = 'k'): VariablesToOpcodeMapper = {
    variables => Register.fromIndex(variables(registerVariable)).map(f(_, variables(valueVariable)))
  }

  def registerOpcode(f: Register => Opcode, registerVariable: Char = 'x'): VariablesToOpcodeMapper = {
    variables => Register.fromIndex(variables(registerVariable)).map(f)
  }

  def registerRegisterOpcode(f: (Register, Register) => Opcode, registerAVariable: Char = 'x', registerBVariable: Char = 'y'): VariablesToOpcodeMapper = {
    variables => {
      for {
        registerAValue <- Register.fromIndex(variables(registerAVariable))
        registerBValue <- Register.fromIndex(variables(registerBVariable))
      } yield {
        f(registerAValue, registerBValue)
      }
    }
  }

  def constantOpcode(o: Opcode): VariablesToOpcodeMapper = _ => Some(o)
}
