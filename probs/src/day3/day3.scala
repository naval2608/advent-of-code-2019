package day3

import java.util

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object day3 {

  //traverse single input like R45 or L3
  def traverseDirectionAndGetEndingIndices(input: String, startingPoint: (Int, Int), arr: Array[Array[String]]) :(Int, Int) = {
    val direction = input.substring(0,1)
    val traversableLength = input.substring(1).toInt
    var (endRowIndex, endColumnIndex) = startingPoint
    var symbol = "-"
    for(i <- 0 to traversableLength - 1) {
      direction match {
        case "R" => endColumnIndex += 1; symbol = "-"
        case "U" => endRowIndex -= 1; symbol = "|"
        case "L" => endColumnIndex -= 1; symbol = "-"
        case "D" => endRowIndex += 1; symbol = "|"
      }
      if(arr(endRowIndex)(endColumnIndex) == ".")
        arr(endRowIndex)(endColumnIndex) = symbol
      else
        arr(endRowIndex)(endColumnIndex) = "X"
    }
    //arr(endRowIndex)(endColumnIndex) = "+"  //can cause confusion if the 2 wires intersect at + so leave it as is.
    //println((endRowIndex, endColumnIndex))
    (endRowIndex, endColumnIndex)
  }


  //traverse single wire input like "R8","U5","L5","D3"
  def traverseWire(wire: Array[String], arr: Array[Array[String]], startingPoint: (Int, Int)) = {
    var endingIndices = startingPoint
    for(path <- wire) {
       endingIndices = traverseDirectionAndGetEndingIndices(path, endingIndices, arr)
    }
  }

  def printArr(arr: Array[Array[String]]) = {
    for(i <- 0 to arr.size-1) {
      for(j <- 0 to arr.size-1) {
        print(arr(i)(j))
      }
      println()
    }
  }

  def assignDot(arr: Array[Array[String]]) = {
    for(i <- 0 to arr.size-1) {
      for(j <- 0 to arr.size-1) {
        arr(i)(j) = "."
      }
    }
  }

  def getIntersections(arr: Array[Array[String]]) : mutable.Queue[(Int, Int)] = {
    val intersections = mutable.Queue[(Int, Int)]()
    for(i <- 0 to arr.size-1) {
      for(j <- 0 to arr.size-1) {
        if(arr(i)(j) == "X") {
          intersections.enqueue((i,j))
        }
      }
    }
    intersections
  }


  def getIntersectionPoints(wire1: Array[String], wire2: Array[String]): List[String]  = {
    val size = 30
    val arr = Array.ofDim[String](size, size)
    assignDot(arr)
    arr(size/2)(size/2) = "O" //assign the central port

    //traverse wire1
    traverseWire(wire1, arr, (size/2, size/2))
    traverseWire(wire2, arr, (size/2, size/2))
    printArr(arr)
    for(intersection <- getIntersections(arr)) {
      println(intersection)
    }
    null
  }

  def main(args: Array[String]): Unit = {
    getIntersectionPoints(Array("R8","U5","L5","D3"), Array("U7","R6","D4","L4"))
  }
}
