package scalashop

import org.scalameter._


object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime")
    println(s"speedup: ${seqtime.value / partime.value}")
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
  // TODO implement this method using the `boxBlurKernel` method
//    val rows = from until end
//    val cols = 0 until src.width
//    var col = cols.start
//    var row = rows.start
//
//    while(rows.contains(row)){
//      while(cols.contains(col) ) {
//        //dst.update(col,row,boxBlurKernel(src,col,row,radius))
//        dst(col,row) = boxBlurKernel(src,col,row,radius)
//
//        col += 1
//      }
//      row += 1
//    }

    for(y <- from until end){
      for(x <- 0 until src.width){
        dst(x,y) = boxBlurKernel(src, x, y, radius)
      }
    }

  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
  // TODO implement using the `task` construct and the `blur` method
  //val listSplittingPoints = 0 to src.width by numTasks
  //val listSplittingPoints = 0 to src.width by (src.width/(Math.min(numTasks, src.width)))
  val listSplittingPoints = 0 to src.height by (src.height/(Math.min(numTasks, src.height)))
    val starEndTuples = listSplittingPoints zip listSplittingPoints.tail

    //starEndTuples.foreach((t) => task(blur(src,dst,t._1,t._2,radius)).join())
    val tasks = starEndTuples.map( { case (from, to) => task(blur(src, dst, from, to, radius)) } )
    tasks foreach {_.join}

  }

}
