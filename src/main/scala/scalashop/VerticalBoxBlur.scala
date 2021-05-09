package org.iyunbo.coding
package scalashop

import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends VerticalBoxBlurInterface {

  /** Blurs the columns of the source image `src` into the destination image
   * `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   * Within each column, `blur` traverses the pixels by going from top to
   * bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (posX <- from until end) {
      for (posY <- 0 until src.height) {
        dst(posX, posY) = boxBlurKernel(src, posX, posY, radius)
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   * Parallelization is done by stripping the source image `src` into
   * `numTasks` separate strips, where each strip is composed of some number of
   * columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val step = if (src.width > numTasks) src.width / numTasks else 1
    val strips: Seq[(Int, Int)] = for {
      start <- 0 until src.width by step
      end = (start + step) min src.width
    } yield (start, end)

    complete(strips map (chunk => task {
      blur(src, dst, chunk._1, chunk._2, radius)
    }))
  }


}
