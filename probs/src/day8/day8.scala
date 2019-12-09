package day8

import scala.io.Source

object day8 {

  def layout(width: Int, tall: Int, digits: Seq[Int]) : Array[Array[Array[Int]]] = {
    val numlayers = digits.size / (tall * width)
    val layer = Array.ofDim[Int](numlayers, tall, width)
    var counter = 0
    def getNextDigit() = {
      digits(counter)
    }

    for (z <- 0 to numlayers-1) {
      print(s"layer:${z + 1}\n")
      for (i <- 0 to tall - 1) {
        for (j <- 0 to width - 1) {
          layer(z)(i)(j) = getNextDigit()
          counter += 1
          print(s"${layer(z)(i)(j)} ")
        }
        println("")
      }
      println()
    }

    layer
  }

  def main(args: Array[String]): Unit = {
    val x = "123456789012".split("").toSeq.map( _.toInt)
    layout(3,2, x)
    println("=======================")

    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day8/input.txt"
    val lst  = Source.fromFile(file).getLines().toList(0).split("").toSeq.map( _.toInt)
    layout(25, 6, lst)
  }
}
