package day8

import scala.collection.mutable.HashMap
import scala.io.Source

case class tracker(Zeros: Int, Ones: Int, Twos: Int)
object day8 {

  def layout(width: Int, tall: Int, digits: Seq[Int]) : Array[Array[Array[Int]]] = {
    val numLayers = digits.size / (tall * width)
    var digitTracker = HashMap.empty[Int, tracker]
    val layer = Array.ofDim[Int](numLayers, tall, width)
    var counter = 0

    def getNextDigit() = {
      digits(counter)
    }

    def incTracker(whichDigit: Int, layerNum: Int) = {
      if(!digitTracker.contains(layerNum)) {
        digitTracker += (layerNum -> new tracker(0,0,0))
      }
      val trc = digitTracker.get(layerNum).get
      whichDigit match {
        case 0 => digitTracker.put(layerNum, new tracker(trc.Zeros+1, trc.Ones, trc.Twos))
        case 1 => digitTracker.put(layerNum, new tracker(trc.Zeros, trc.Ones+1, trc.Twos))
        case 2 => digitTracker.put(layerNum, new tracker(trc.Zeros, trc.Ones, trc.Twos+1))
        case _ => //meh
      }
    }

    def printLayerDetails(whichLayer: Int) = {
      println(s"Layer: ${whichLayer+1}, 0's: ${digitTracker.get(whichLayer).get.Zeros}, 1's: ${digitTracker.get(whichLayer).get.Ones}, 2's: ${digitTracker.get(whichLayer).get.Twos}")
    }

    var layerWithMinZeros = 0
    var numOfMinZeros = Int.MaxValue
    for (z <- 0 until numLayers) {
      print(s"layer:${z + 1}\n")
      for (i <- 0 until tall) {
        for (j <- 0 until width) {
          layer(z)(i)(j) = getNextDigit()

          getNextDigit() match {
            case a@(0 | 1 | 2) => incTracker(a, z)
            case _ => //meh
          }
          counter += 1
          print(s"${layer(z)(i)(j)} ")
        }
        println("")
      }
      printLayerDetails(z)

      if(digitTracker.get(z).get.Zeros < numOfMinZeros) {
        //println(s"got layer with min zeros: ${digitTracker.get(z).get.Zeros}, prev layer: ")
        layerWithMinZeros = z
        numOfMinZeros = digitTracker.get(z).get.Zeros
      }
      println()
    }
    println(s"Minimum zeros in layer ${layerWithMinZeros+1}, 1's * 2's = ${digitTracker.get(layerWithMinZeros).get.Ones *  digitTracker.get(layerWithMinZeros).get.Twos}")
    layer
  }

  def main(args: Array[String]): Unit = {
    val x = "123250122112123456".split("").toSeq.map( _.toInt)
    layout(3,2, x)
    println("=======================")

    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day8/input.txt"
    val lst  = Source.fromFile(file).getLines().toList(0).split("").toSeq.map( _.toInt)
    layout(25, 6, lst)
  }
}
