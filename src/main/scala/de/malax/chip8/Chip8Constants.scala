package de.malax.chip8
import scodec.bits._

object Chip8Constants {
  val fontCharacterZero = hex"F0909090F0"
  val fontCharacterOne = hex"2060202070"
  val fontCharacterTwo = hex"F010F080F0"
  val fontCharacterThree = hex"F010F010F0"
  val fontCharacterFour = hex"9090F01010"
  val fontCharacterFive = hex"F080F010F0"
  val fontCharacterSix = hex"F080F090F0"
  val fontCharacterSeven = hex"F010204040"
  val fontCharacterEight = hex"F090F090F0"
  val fontCharacterNine = hex"F090F010F0"
  val fontCharacterA = hex"F090F09090"
  val fontCharacterB = hex"E090E090E0"
  val fontCharacterC = hex"F0808080F0"
  val fontCharacterD = hex"E0909090E0"
  val fontCharacterE = hex"F080F080F0"
  val fontCharacterF = hex"F080F08080"

  val memoryInterpreterArea =
    fontCharacterZero ++ fontCharacterOne ++ fontCharacterTwo ++ fontCharacterThree ++ fontCharacterFour ++ fontCharacterFive ++ fontCharacterSix ++
    fontCharacterSeven ++ fontCharacterEight ++ fontCharacterNine ++ fontCharacterA ++ fontCharacterB ++ fontCharacterC ++ fontCharacterD ++
    fontCharacterE ++ fontCharacterF
}
