package de.malax.chip8.utils

import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.BitVector
import scodec.bits._

class DisplayUtilsTest extends FlatSpec with Matchers {

  "DisplayUtils#drawSprite" should "draw a sprite at 0,0 correctly" in {
    val display = BitVector.fill(10 * 10)(high = false)
    val modifiedDisplay = DisplayUtils.drawSprite(0, 0, ByteVector.fromByte(89), 10, 10, display)
    modifiedDisplay._1.getByte(0) should be (89)
  }

  private def byteFromDisplay(pixelOffset: Int, display: BitVector): Int = {
    0.until(8).foldLeft(0x0) { (acc, index) =>
      val offset = (pixelOffset + index) % 64
      val value = if (display.get(offset)) 1 else 0
      (acc << 1) | value
    }
  }

  private def printDisplay(display: BitVector): Unit = {
    for (i <- 0 until 64 * 32) {
      if (i != 0 && i % 64 == 0) println()
      print(if (display.get(i)) "X" else "_")
    }
  }
}
