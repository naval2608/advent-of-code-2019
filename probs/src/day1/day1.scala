package day1

import scala.io.Source

object day1 {

  def calculateFuel(mass: Long) : Long = {
    //println(s"SSS:$mass -> ${(mass/3.toFloat).floor.toLong-2}")
    (mass/3.toFloat).floor.toLong-2
  }

  def printCalculateFuel(mass: List[String]) : Long = {
    var sum = 0.toLong
    for(ms <- mass) {
      println(s"$ms -> ${calculateFuel(ms.toLong)}")
      sum += calculateFuel(ms.toLong)
    }
    sum
  }

  def printCalculateFuelLong(mass: List[Long]) : Long = {
    var sum = 0.toLong
    for(ms <- mass) {
      println(s"$ms -> ${calculateFuel(ms)}")
      sum += calculateFuel(ms.toLong)
    }
    sum
  }

  def calculateFuelPart2(mass: Long) : Long = {
    var sum = 0.toLong
    var cur = calculateFuel(mass)
    while(cur > 0.toLong) {
      sum += cur
      cur = calculateFuel(cur)
    }
    println(s"Final $mass -> $sum")
    sum
  }

  def printCalculatePart2(mass: List[Long]) : Long = {
    var sum = 0.toLong
    for(ms <- mass) {
      //println(s"$ms -> ${calculateFuelPart2(ms)}")
      sum += calculateFuelPart2(ms.toLong)
    }
    sum
  }

  def printCalculatePart2Long(mass: List[String]) : Long = {
    var sum = 0.toLong
    for(ms <- mass) {
      //println(s"$ms -> ${calculateFuelPart2(ms)}")
      sum += calculateFuelPart2(ms.toLong)
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day1/input-day1.txt"
    var sum = 0.toLong
    sum += printCalculatePart2Long(Source.fromFile(file).getLines.toList)
    //sum += printCalculatePart2(List(12,14,1969,100756))
    println(s"Final TOTAL -> $sum")
  }
}
