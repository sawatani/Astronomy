package org.fathens.moon

import java.nio.file.Paths
import java.nio.file.Path
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object MoonPhases extends App with Logger {
  Log debug f"Arguments: ${args}"
  args.length match {
    case 1 =>
      (0 until 30).foreach { phase =>
        val target = {
          val value = f"0${phase}".takeRight(2)
          val filename = f"phase-${value}.png"
          Paths.get(args(0), filename)
        }
        save(makeImage(phase), target)
      }
    case 2 =>
      val target = Paths get args(0)
      val phase = args(1).toInt
      save(makeImage(phase), target)
    case _ =>
      Log fatal "Artguments are miss match"
  }

  def save(image: BufferedImage, file: Path) {
    val name = file.getFileName.toFile.getName
    val ext = if (name contains '.') name.reverse.takeWhile(_ != '.').reverse else "png"
    ImageIO.write(image, ext, file.toFile)
  }
  def makeImage(phase: Int): BufferedImage = {
    assert(0 <= phase && phase < 30)
    null
  }
}
