package org.iyunbo.coding

import scalashop.{Img, VerticalBoxBlur, boxBlurKernel}

import org.junit.Assert.assertEquals
import org.junit.{Rule, Test}

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  @Test def should_create_an_image(): Unit = {
    val img = makeImage(24, 24)
    assertEquals(img(0, 0), 0)
    img(23, 23) = 256
    assertEquals(img(23, 23), 256)
    assertEquals(img.width, 24)
    assertEquals(img.height, 24)
  }

  @Test def should_blur_an_image(): Unit = {
    val img: Img = makeImage(24, 24)
    assertEquals(10, boxBlurKernel(img, 10, 10, 1))
    assertEquals(1, boxBlurKernel(img, 1, 1, 1))
  }

  private def makeImage(size: (Int, Int)) = {
    val img = new Img(size._1, size._2)
    for (x <- 0 until size._1) {
      for (y <- 0 until size._2) {
        img(x, y) = x
      }
    }
    img
  }

  @Test def should_blur_vertically(): Unit = {
    val boxBlur = VerticalBoxBlur
    val img = makeImage(10, 10)
    val dst = makeImage(10, 10)
    boxBlur.blur(img, dst, 0, 5, 1)
    assertEquals(2, dst(2, 2))
    assertEquals(9, dst(9, 9))
  }
}
