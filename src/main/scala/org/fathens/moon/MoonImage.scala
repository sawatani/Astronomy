package org.fathens.moon

import java.awt.image.BufferedImage
import java.nio.file.{Path, Paths}

import javax.imageio.ImageIO
import com.typesafe.scalalogging.LazyLogging

object MoonImage extends App with LazyLogging {
  val max = 30
  logger warn f"Arguments: ${args mkString ", "}"
  args.toList match {
    case r :: target :: phase :: Nil => save(makeImage(r.toInt, phase.toInt), Paths get target)
    case r :: dir :: Nil => (0 until max).foreach { phase =>
      val target = {
        val value = f"0${phase}".takeRight(2)
        val filename = f"phase-${value}.png"
        Paths.get(dir, filename)
      }
      save(makeImage(r.toInt, phase), target)
    }
    case _ => logger error "Artguments are miss match"
  }

  def save(image: BufferedImage, file: Path) {
    val name = file.getFileName.toFile.getName
    val ext = if (name contains '.') name.reverse.takeWhile(_ != '.').reverse else "png"
    logger info f"Writing image(${ext}) to ${file}"
    file.getParent.toFile.mkdirs
    ImageIO.write(image, ext, file.toFile)
  }
  def makeImage(r: Int, phase: Int): BufferedImage = {
    assert(0 <= phase && phase < max)
    val on = SphereSilhouette.on(r)(max)(phase)_
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
      logger trace f"Phase: ${phase}: Pixel(${x}, ${y}) = ${rgba mkString ", "}"
      raster.setPixel(x + r, y + r, rgba)
    }
    image
  }
}
