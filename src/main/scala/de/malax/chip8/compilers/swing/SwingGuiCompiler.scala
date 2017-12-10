package de.malax.chip8.compilers.swing

import java.awt.Color
import java.awt.event.{KeyEvent, WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import cats.effect.IO
import cats.~>
import de.malax.chip8._
import de.malax.chip8.operations._
import de.malax.chip8.utils.DisplayUtils
import scodec.bits.{BitVector, ByteVector}

import scala.collection.mutable

object SwingGuiCompiler {
  private val displayPixelSize = 10

  private val keyMapping = Map[Key, Int](
    Key1 -> KeyEvent.VK_1,
    Key2 -> KeyEvent.VK_2,
    Key3 -> KeyEvent.VK_3,
    KeyC -> KeyEvent.VK_4,

    Key4 -> KeyEvent.VK_Q,
    Key5 -> KeyEvent.VK_W,
    Key6 -> KeyEvent.VK_E,
    KeyD -> KeyEvent.VK_R,

    Key7 -> KeyEvent.VK_A,
    Key8 -> KeyEvent.VK_S,
    Key9 -> KeyEvent.VK_D,
    KeyE -> KeyEvent.VK_F,

    KeyA -> KeyEvent.VK_Y,
    Key0 -> KeyEvent.VK_X,
    KeyB -> KeyEvent.VK_C,
    KeyF -> KeyEvent.VK_V
  )

  val compiler: MachineOperationA ~> IO = new (MachineOperationA ~> IO) {
    val window = new JFrame("Chip 8 Emulator")
    val displayBufferedImage = new BufferedImage(64 * displayPixelSize, 32 * displayPixelSize, BufferedImage.TYPE_INT_RGB)
    val label = new JLabel(new ImageIcon(displayBufferedImage))
    window.getContentPane.add(label)

    window.setSize(800, 600)
    window.setVisible(true)

    val ki = new KeyboardInput()
    window.addKeyListener(ki)

    var pc: Int = 0x200
    var display = BitVector.fill(64 * 32)(high = false)
    var memory = ByteVector.fill(0xFFF)(0x0).patch(0, Chip8Constants.memoryInterpreterArea)
    var stack = List[Int]()
    val registers = mutable.Map[Register, Int](
      V0 -> 0x0, V1 -> 0x0, V2 -> 0x0, V3 -> 0x0, V4 -> 0x0, V5 -> 0x0, V6 -> 0x0, V7 -> 0x0, V8 -> 0x0, V9 -> 0x0,
      VA -> 0x0, VB -> 0x0, VC -> 0x0, VD -> 0x0, VE -> 0x0, VF -> 0x0, I -> 0x0, DT -> 0x0, ST -> 0x0
    )

    var lastCycle = System.currentTimeMillis()

    var timerAcc = 0L
    var powerState = true

    window.addWindowListener(new WindowAdapter {
      override def windowClosed(e: WindowEvent) = {
        powerState = true
        window.dispose()
      }
    })

    override def apply[A](fa: MachineOperationA[A]) = {
      ki.poll()
      if (ki.keyDown(KeyEvent.VK_ESCAPE)) {
        powerState = false
        window.dispose()
      }

      val timePassed = System.currentTimeMillis() - lastCycle
      timerAcc = timerAcc + timePassed

      val dTValue = registers(DT)
      if (timerAcc > 1000 / 60) {
        timerAcc = timerAcc - (1000 / 60)

        if (dTValue > 0) {
          registers.update(DT, dTValue - 1)
        }
      }

      fa match {
        case PatchMemory(address, bytes) => IO[Unit](memory = memory.patch(address, bytes))
        case ClearScreen => IO[Unit](display = BitVector.fill(64 * 32)(high = false))
        case PushStack(address) => IO[Unit](stack = address :: stack)
        case PopStack => IO[Int]({
          val head :: tail = stack
          stack = tail
          head
        })
        case GenerateRandom => IO(math.random().toInt & 0xFF)
        case DrawSprite(x, y, spriteData) =>
          IO {
            val (modifiedDisplay, collision) = DisplayUtils.drawSprite(x, y, spriteData, 64, 32, display)
            display = modifiedDisplay
            updateBufferedImage(displayBufferedImage, modifiedDisplay)
            label.repaint()
            collision
          }

        case GetKeyDown(key) => {
          IO(ki.keyDown(keyMapping(key)))
        }

        case ReadByte(address) => IO(memory.get(address) & 0xFF)
        case WriteByte(address, value) => IO[Unit](memory = memory.patch(address, ByteVector(value.toByte)))
        case ReadRegister(register) => IO(registers(register))
        case WriteRegister(register, byte) => IO(registers.update(register, byte))
        case ReadPc => IO.pure(pc)
        case WritePc(address) => IO[Unit](pc = address)
        case ReadPowerState => IO.pure(powerState)
      }
    }
  }

  private def updateBufferedImage(bufferedImage: BufferedImage, bitVector: BitVector): Unit = {
    for (x <- 0 until 64; y <- 0 until 32) {
      for (iX <- 0 until displayPixelSize; iY <- 0 until displayPixelSize) {
        bufferedImage.setRGB(x * displayPixelSize + iX, y * displayPixelSize + iY, if (bitVector.get(y * 64 + x)) Color.GREEN.getRGB else Color.BLACK.getRGB)
      }
    }
  }
}

