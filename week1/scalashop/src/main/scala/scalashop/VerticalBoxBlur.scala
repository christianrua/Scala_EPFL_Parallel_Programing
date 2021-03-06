package scalashop

import org.scalameter._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

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
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method
    val rows = 0 until src.height
    val cols = from to end
    var col = cols.start
    var row = rows.start

    while(cols.contains(col)){
      while(rows.contains(row)) {
        //dst.update(col,row,boxBlurKernel(src,col,row,radius))
        dst(col,row) = boxBlurKernel(src,col,row,radius)
        row += 1
      }
      col += 1
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    //val listSplittingPoints = 0 to src.width by numTasks
    val listSplittingPoints = 0 to src.width by (src.width/(Math.min(numTasks, src.width)))
    val starEndTuples = listSplittingPoints zip listSplittingPoints.tail

    //starEndTuples.foreach((t) => task(blur(src,dst,t._1,t._2,radius)).join())
    val tasks = starEndTuples.map( { case (from, to) => task(blur(src, dst, from, to, radius)) } )
    tasks foreach {_.join}
  }

}
