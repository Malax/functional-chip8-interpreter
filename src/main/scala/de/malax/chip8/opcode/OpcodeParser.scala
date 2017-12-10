package de.malax.chip8.opcode

import de.malax.chip8.Register

object OpcodeParser {
  def parse(opcode: Int): Option[Opcode] = {
    opcodeMappers.iterator.map(_(opcode)).find(_.isDefined).flatten
  }

  import VariablesToOpcodeMapper._
  private val opcodeMappers = List(
    BinaryToOpcodeMapper.build("0E00")(constantOpcode(ClearScreen)),
    BinaryToOpcodeMapper.build("00EE")(constantOpcode(Return)),
    BinaryToOpcodeMapper.build("1nnn")(valueOpcode(JumpToAddress)),
    BinaryToOpcodeMapper.build("2nnn")(valueOpcode(CallSubroutine)),
    BinaryToOpcodeMapper.build("3xkk")(registerValueOpcode(SkipIfEqual)),
    BinaryToOpcodeMapper.build("4xkk")(registerValueOpcode(SkipIfNotEqual)),
    BinaryToOpcodeMapper.build("5xy0")(registerRegisterOpcode(SkipIfEqualRegister)),
    BinaryToOpcodeMapper.build("6xkk")(registerValueOpcode(LoadConstant)),
    BinaryToOpcodeMapper.build("7xkk")(registerValueOpcode(AddConstant)),
    BinaryToOpcodeMapper.build("8xy0")(registerRegisterOpcode(LoadRegister)),
    BinaryToOpcodeMapper.build("8xy1")(registerRegisterOpcode(OrRegister)),
    BinaryToOpcodeMapper.build("8xy2")(registerRegisterOpcode(AndRegister)),
    BinaryToOpcodeMapper.build("8xy3")(registerRegisterOpcode(XorRegister)),
    BinaryToOpcodeMapper.build("8xy4")(registerRegisterOpcode(AddRegister)),
    BinaryToOpcodeMapper.build("8xy5")(registerRegisterOpcode(SubtractRegister)),
    BinaryToOpcodeMapper.build("8xy6")(registerOpcode(ShiftRight)),
    BinaryToOpcodeMapper.build("8xy7")(registerRegisterOpcode(SubN)),
    BinaryToOpcodeMapper.build("8xy8")(registerOpcode(ShiftLeft)),
    BinaryToOpcodeMapper.build("9xy0")(registerRegisterOpcode(SkipIfNotEqualRegister)),
    BinaryToOpcodeMapper.build("Annn")(valueOpcode(LoadIRegister)),
    BinaryToOpcodeMapper.build("Bnnn")(valueOpcode(JumpToAddressOffsettedByVa)),
    BinaryToOpcodeMapper.build("Dxyn") { variables =>
      for {
        r1 <- Register.fromIndex(variables('x'))
        r2 <- Register.fromIndex(variables('y'))
      } yield DrawSprite(r1, r2, variables('n'))
    },
    BinaryToOpcodeMapper.build("Cxkk")(registerValueOpcode(GenerateRandom)),
    BinaryToOpcodeMapper.build("Ex9E")(registerOpcode(SkipIfKeyPressed)),
    BinaryToOpcodeMapper.build("ExA1")(registerOpcode(SkipIfKeyNotPressed)),
    BinaryToOpcodeMapper.build("Fx07")(registerOpcode(ReadTimerValue)),
    BinaryToOpcodeMapper.build("Fx0A")(registerOpcode(LoadPressedKey)),
    BinaryToOpcodeMapper.build("Fx15")(registerOpcode(LoadTimer)),
    BinaryToOpcodeMapper.build("Fx18")(registerOpcode(LoadSoundTimer)),
    BinaryToOpcodeMapper.build("Fx1E")(registerOpcode(AddRegisterValueToI)),
    BinaryToOpcodeMapper.build("Fx29")(registerOpcode(LoadSpriteAddressForDigit)),
    BinaryToOpcodeMapper.build("Fx33")(registerOpcode(BinaryCodedDecimal)),
    BinaryToOpcodeMapper.build("Fx55")(registerOpcode(WriteRegistersToMemory)),
    BinaryToOpcodeMapper.build("Fx65")(registerOpcode(ReadRegistersFromMemory)),
  )
}
