package org.fathens.moon

import java.nio.file.Paths
import java.nio.file.Path
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object MoonPhases extends App with Logger {
  Log debug f"Arguments: ${args mkString ", "}"
  args.toList match {
    case r :: target :: phase :: Nil => save(makeImage(r.toInt, phase.toInt), Paths get target)
    case r :: dir :: Nil => (0 until 30).foreach { phase =>
      val target = {
        val value = f"0${phase}".takeRight(2)
        val filename = f"phase-${value}.png"
        Paths.get(dir, filename)
      }
      save(makeImage(r.toInt, phase), target)
    }
    case _ => Log fatal "Artguments are miss match"
  }

  def save(image: BufferedImage, file: Path) {
    val name = file.getFileName.toFile.getName
    val ext = if (name contains '.') name.reverse.takeWhile(_ != '.').reverse else "png"
    ImageIO.write(image, ext, file.toFile)
  }
  def makeImage(r: Int, phase: Int): BufferedImage = {
    assert(0 <= phase && phase < 30)
    val on = SphereSilhouette.on(r)(30)(phase)_
    val image = new BufferedImage(r * 2, r * 2, BufferedImage.TYPE_INT_ARGB)
    val raster = image.getRaster
    for {
      y <- (-r until r)
      x <- (-r until r)
    } {
      val rgba = on(x, y) match {
        case None        => Array(0, 0, 0, 0)
        case Some(false) => Array(0, 0, 0, 255)
        case Some(true)  => Array(255, 255, 0, 255)
      }
      Log info f"Phase: ${phase}: Pixel(${x}, ${y}) = ${rgba mkString ", "}"
      raster.setPixel(x + r, y + r, rgba)
    }
    image
  }
}
