package de.malax.chip8.utils

import org.scalatest.{FlatSpec, Matchers}

class BinaryPatternSpec extends FlatSpec with Matchers {

  "compileNibblePatternFromString" should "work with 4bit variables" in {
    BinaryPattern.compileNibblePatternFromString("Fx65").matchBinary(0xF565) should be (Some(Map('x' -> 5)))
  }

  it should "work with 8bit variables" in {
    BinaryPattern.compileNibblePatternFromString("3xkk").matchBinary(0x3565) should be (Some(Map('x' -> 0x5, 'k' -> 0x65)))
  }

  it should "work with 12bit variables" in {
    BinaryPattern.compileNibblePatternFromString("Dnnn").matchBinary(0xDEAD) should be (Some(Map('n' -> 0xEAD)))
  }

  it should "report non-matching patterns" in {
    BinaryPattern.compileNibblePatternFromString("Fnnn").matchBinary(0xDEAD) should be (None)
  }

  it should "work with patterns that have no variables" in {
    BinaryPattern.compileNibblePatternFromString("00C0").matchBinary(0x00C0) should be (Some(Map.empty))
  }

  it should "work with upper and lowercase constants" in {
    BinaryPattern.compileNibblePatternFromString("fCvv").matchBinary(0xFC23) should be (Some(Map('v' -> 0x23)))
  }

  it should "work for patterns consisting only of variables" in {
    BinaryPattern.compileNibblePatternFromString("xxyy").matchBinary(0xFC23) should be (Some(Map('x' -> 0xFC, 'y' -> 0x23)))
  }

  it should "not confuse patterns" in {
    BinaryPattern.compileNibblePatternFromString("2nnn").matchBinary(0x6A02) should be (None)
  }

  it should "fail for patterns that are too long" in {
    an [IllegalArgumentException] should be thrownBy {
      BinaryPattern.compileNibblePatternFromString("FFFFFFFF00")
    }
  }

  it should "fail for patterns that are too short" in {
    an [IllegalArgumentException] should be thrownBy {
      BinaryPattern.compileNibblePatternFromString("")
    }
  }

  it should "fail for patterns with disjointed variable pattern" in {
    an [IllegalArgumentException] should be thrownBy {
      BinaryPattern.compileNibblePatternFromString("FkFk")
    }
  }
}
