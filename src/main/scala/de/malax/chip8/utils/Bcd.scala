package de.malax.chip8.utils

object Bcd {
  def encode(byte: Int): (Int, Int, Int) = {
    val x = "%03d".format(byte).toCharArray.map(_ - '0')
    (x(0), x(1), x(2))
  }
}
