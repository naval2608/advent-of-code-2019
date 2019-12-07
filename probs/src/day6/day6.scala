package day6

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.io.Source
import util.control.Breaks._

object day6 {

  val orbits = HashMap.empty[String, String]

  def contructOrbital(path: String): Unit = {
    val paths = path.split(')').toSeq
    orbits += (paths(1) -> paths(0))
  }

  def orbitsForObject(obj: String) : ListBuffer[String] = {
    obj match {
      case "COM" => ListBuffer.empty[String]
      case _ => (ListBuffer(orbits(obj)) ++ orbitsForObject(orbits(obj)))
    }
  }

  def main(args: Array[String]): Unit = {
    //val map = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L".split("\n")
    //val map = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN".split("\n")
    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day6/input.txt"
    val map = Source.fromFile(file).getLines().toList
    var totalOrbits = 0
    map.foreach(path => contructOrbital(path))
    orbits.keySet.foreach(objj => {
      val lst = orbitsForObject(objj)
      totalOrbits += lst.size
      println(s"$objj -> orbits: ${lst.mkString(",")} Size: ${lst.size}")
    })

    println(s"totalOrbits: $totalOrbits")

    println(orbitsForObject("YOU").mkString(","))
    println(orbitsForObject("SAN").mkString(","))

    val orbitsForYou = orbitsForObject("YOU")
    val orbitsForSan = orbitsForObject("SAN")
    var hco: String = "" //highest common orbiter
    var hopCounter = 0
    breakable {
      orbitsForYou.foreach(
        objj => {
          hopCounter += 1
          println(s"hopped $objj for YOU, hop counter $hopCounter")
          if (orbitsForSan.contains(objj)) {
            println(s"Found ${objj}")
            hco = objj
            break()
          }

        }
      )
    }

    breakable {
      orbitsForSan.foreach(
        objj => {
          hopCounter += 1
          println(s"hopped $objj for SAN, hop counter $hopCounter")
          if (objj.equals(hco)) {
            println(s"Found ${hco}")
            break()
          }

        }
      )
    }
    println(s"hopcounter : ${hopCounter-2}")

  }
}
