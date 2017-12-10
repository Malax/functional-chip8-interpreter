package de.malax.chip8.utils

import scala.collection.Seq
import scala.util.Try

trait BinaryPattern {
  def matchBinary(binary: Int): Option[Map[Char, Int]]
}

object BinaryPattern {
  /**
    * Compiles a binary pattern from a string.
    *
    * Every character in the string is treated as a nibble. This means that you can only
    * match on multiples of 4 bits.
    *
    * Example A: The pattern "FzzA" would match on 0xF23A and returns 0x23 as variable z.
    * Example B: The pattern "kkk9" would match on 0x6789 and returns 0x678 as variable k.
    *
    * This function does all expensive calculations so that the resulting BinaryPattern can
    * be executed as fast as possible.
    *
    * @param stringPattern The pattern to compile
    * @return A BinaryPattern for the given string pattern
    */
  def compileNibblePatternFromString(stringPattern: String): BinaryPattern = {
    require(stringPattern.length > 0 && stringPattern.length <= 8, "Pattern length must be between 0 and 8!")

    val baseOpcodePattern = stringPattern.foldLeft(0x0) { (pattern, char) =>
      val patternNibble = Try(Integer.parseInt(char.toString, 16)).getOrElse(0)
      (pattern << 4) | patternNibble
    }

    val baseOpcodeMask = stringPattern.foldLeft(0x0) { (pattern, char) =>
      val patternNibble = Try(Integer.parseInt(char.toString, 16)).map(_ => 0xF).getOrElse(0)
      (pattern << 4) | patternNibble
    }

    val nibbleIndexesByVariable = stringPattern
      .zipWithIndex
      .groupBy(_._1)
      .filterKeys(char => Try(Integer.parseInt(char.toString, 16)).isFailure)
      .mapValues(_.map(_._2))
      .mapValues(_.map(idx => stringPattern.length - 1 - idx))

    require(nibbleIndexesByVariable.values.forall(isConsecutive), "Variable patterns must be consecutive!")

    val patternsByVariable = nibbleIndexesByVariable mapValues { nibbleIndexes =>
      val variableValuePattern = nibbleIndexes.foldLeft(0x0) { (pattern, index) =>
        pattern | (0xF << index * 4)
      }

      val shiftRight = nibbleIndexes.min * 4

      (variableValuePattern, shiftRight)
    }

    (value) => {
      Option(value).filter(value => (value & baseOpcodeMask) == baseOpcodePattern) map { value =>
        patternsByVariable mapValues { kv =>
          val (pattern, rightShifts) = kv
          (value & pattern) >> rightShifts
        }
      }
    }
  }

  private def isConsecutive(l: Seq[Int]): Boolean = {
    l.sorted.sliding(2) forall { window =>
      if (window.size > 1) {
        window.head + 1 == window(1)
      } else {
        true
      }
    }
  }
}
