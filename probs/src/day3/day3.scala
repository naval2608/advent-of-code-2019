package day3

import scala.collection.mutable
import scala.io.Source

object day3 {

  val size = 30000
  val wire1Traversal = new mutable.HashMap[(Int, Int), Int]
  val wire2Traversal = new mutable.HashMap[(Int, Int), Int]

  //traverse single input like R45 or L3
  def traverseDirectionAndGetEndingIndices(input: String, startingPoint: (Int, Int), arr: Array[Array[Char]], wireInfo: Int) :(Int, Int) = {
    val direction = input.substring(0,1)
    val traversableLength = input.substring(1).toInt
    var (endRowIndex, endColumnIndex) = startingPoint
    var symbol = wireInfo.toString.charAt(0)
    for(i <- 0 to traversableLength - 1) {
      direction match {
        case "R" => endColumnIndex += 1//; symbol = '-'
        case "U" => endRowIndex -= 1//; symbol = '|'
        case "L" => endColumnIndex -= 1//; symbol = '-'
        case "D" => endRowIndex += 1//; symbol = '|'
      }
      wireInfo match {
        case 1 => {
          if(wire1Traversal.contains((endRowIndex, endColumnIndex))){
            wire1Traversal.put((endRowIndex, endColumnIndex), wire1Traversal.get((endRowIndex, endColumnIndex)).get+1)
          } else {
            wire1Traversal += (endRowIndex, endColumnIndex) -> 1
          }
        }
        case 2 => {
          if(wire2Traversal.contains((endRowIndex, endColumnIndex))){
            wire2Traversal.put((endRowIndex, endColumnIndex), wire2Traversal.get((endRowIndex, endColumnIndex)).get+1)
          } else {
            wire2Traversal += (endRowIndex, endColumnIndex) -> 1
          }
        }
      }

      if(arr(endRowIndex)(endColumnIndex) == '.')
        arr(endRowIndex)(endColumnIndex) = symbol
      else {

        if(wireInfo == 1 && wire1Traversal.get(endRowIndex, endColumnIndex).get > 1) {
          arr(endRowIndex)(endColumnIndex) = '1'
          println(s"wire1 crossing itself again at ${(endRowIndex, endColumnIndex)}")


        } else if(wireInfo == 2 && wire2Traversal.get(endRowIndex, endColumnIndex).get > 1) {
          arr(endRowIndex)(endColumnIndex) = '2'
          println(s"wire2 crossing itself again at ${(endRowIndex, endColumnIndex)}")
        } else arr(endRowIndex)(endColumnIndex) = 'X'
      }
    }
    //arr(endRowIndex)(endColumnIndex) = "+"  //can cause confusion if the 2 wires intersect at + so leave it as is.
    //println((endRowIndex, endColumnIndex))
    (endRowIndex, endColumnIndex)
  }


  //traverse single wire input like "R8","U5","L5","D3"
  def traverseWire(wire: Array[String], arr: Array[Array[Char]], startingPoint: (Int, Int), wireInfo: Int) = {
    var endingIndices = startingPoint
    var i =0
    for(path <- wire) {
      endingIndices = traverseDirectionAndGetEndingIndices(path, endingIndices, arr, wireInfo)
      i += 1
      //println(s"iteration: $i, path: $path, indices: ${endingIndices}")
    }
  }

  def printArr(arr: Array[Array[Char]]) = {
    for(i <- 0 to arr.size-1) {
      for(j <- 0 to arr.size-1) {
        print(arr(i)(j))
      }
      println()
    }
  }

  def assignDot(arr: Array[Array[Char]]) = {
    for(i <- 0 to arr.size-1) {
      for(j <- 0 to arr.size-1) {
        arr(i)(j) = '.'
      }
    }
  }

  def getIntersections(arr: Array[Array[Char]]) : mutable.Queue[(Int, Int)] = {
    val intersections = mutable.Queue[(Int, Int)]()
    for(i <- 0 to arr.size-1) {
      for(j <- 0 to arr.size-1) {
        if(arr(i)(j) == 'X') {
          intersections.enqueue((i,j))
        }
      }
    }
    intersections
  }


  def getIntersectionPoints(wire1: Array[String], wire2: Array[String]): Int  = {
    val arr = Array.ofDim[Char](size, size)
    assignDot(arr)
    arr(size/2)(size/2) = 'O' //assign the central port
    val centre = (size/2,size/2)
    //traverse wires
    traverseWire(wire1, arr, centre, 1)
    traverseWire(wire2, arr, centre, 2)
    //printArr(arr)
    val distanceList = new scala.collection.mutable.ListBuffer[Int]
    println(s"\n\ncentral point -> ${centre}")
    for(intersection <- getIntersections(arr)) {
      val ds = (centre._1 - intersection._1).abs + (centre._2 - intersection._2).abs
      distanceList += ds
      println(s"Intersection -> $intersection Distance -> $ds")
    }
    println(s"minimum DS -> ${distanceList.min}")
    distanceList.min
  }

  def main(args: Array[String]): Unit = {
    getIntersectionPoints(Array("R8","U5","L5","D3"), Array("U7","R6","D4","L4"))

    println("===================")
    getIntersectionPoints(Array("R75","D30","R83","U83","L12","D49","R71","U7","L72"), Array("U62","R66","U55","R34","D71","R55","D58","R83"))

    println("===================")
    getIntersectionPoints(Array("R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"),
      Array("U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"))

    println("===================")
    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day3/input-day3.txt"
    val wire1 = Source.fromFile(file).getLines().toList(0).split(",")
    val wire2 = Source.fromFile(file).getLines().toList(1).split(",")
    getIntersectionPoints(wire1, wire2)
  }
}
