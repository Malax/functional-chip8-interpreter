package de.malax.chip8.utils

import scodec.bits.{BitVector, ByteVector}

object DisplayUtils {

  def drawSprite(x: Int, y: Int, spriteData: ByteVector, width: Int, height: Int, displayData: BitVector): (BitVector, Boolean) = {
    var modifiedDisplay = displayData
    var pixelErased = false

    for (line <- 0 until spriteData.length.toInt) {
      val yPos = (y + line) % height
      val (d, c) = drawLine(x, yPos, spriteData.get(line), width, height, modifiedDisplay)

      modifiedDisplay = d
      if (c) {
        pixelErased = true
      }
    }

    (modifiedDisplay, pixelErased)
  }

  def drawLine(x: Int, y: Int, line: Int, width: Int, height: Int, display: BitVector): (BitVector, Boolean) = {
    var modifiedDisplay = display
    var pixelErased = false

    for (i <- 0 until 8) {
      val pixelMask = 0x80 >> i
      val pixelValue = (line & pixelMask) == pixelMask

      val pixelDisplayIndex = (y * width) + ((x + i) % width)
      val oldPixel = modifiedDisplay.get(pixelDisplayIndex)
      val newPixel = oldPixel ^ pixelValue

      modifiedDisplay = if (newPixel) modifiedDisplay.set(pixelDisplayIndex) else modifiedDisplay.clear(pixelDisplayIndex)

      if (oldPixel && newPixel) {
        pixelErased = true
      }
    }

    (modifiedDisplay, pixelErased)
  }
}
