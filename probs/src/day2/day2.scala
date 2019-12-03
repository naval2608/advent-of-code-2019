package day2

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object day2 {

  def genIntcode(lst : ListBuffer[Long]): ListBuffer[Long] = {
    var keepGoing = true

    def doOperation(lstBuffer : ListBuffer[Long]) :
      ListBuffer[Long] = {
      lstBuffer(0) match {
        case 1 =>  {
          lst(lstBuffer(3).toInt) = lst(lstBuffer(1).toInt) + lst(lstBuffer(2).toInt)
          //println(lst.mkString(","))
        }
        case 2 =>  {
          lst(lstBuffer(3).toInt) = lst(lstBuffer(1).toInt) * lst(lstBuffer(2).toInt)
          //println(lst.mkString(","))
        }
        case 99 =>  {
          keepGoing = false
          //println("HALT::" + lst.mkString(","))
        }
        //case _ => //no op
      }
      lst
    }

    var (start, increment) = (0, 4)
    while(keepGoing) {
      val sublist = lst.slice(start, start + increment)
      doOperation(sublist)
      start = start + increment
    }
    lst
  }

  def main(args: Array[String]): Unit = {
    genIntcode(ListBuffer(1,0,0,0,99))
    genIntcode(ListBuffer(2,3,0,3,99))
    genIntcode(ListBuffer(2,4,4,5,99,0))
    genIntcode(ListBuffer(1,1,1,4,99,5,6,0,99))
    genIntcode(ListBuffer(1,9,10,3,2,3,11,0,99,30,40,50))

    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day2/input-day2.txt"
    val lst  = Source.fromFile(file).getLines().toList(0).split(",").map(_.toLong).to[ListBuffer]

    lst(1)=23
    lst(2)=23
    println(genIntcode(lst).mkString(","))
    println;println
    for(i <-  Range(0,100)) {
      for(j <- Range(0,100)) {
        val lstPart2 = Source.fromFile(file).getLines().toList(0).split(",").map(_.toLong).to[ListBuffer]
        lstPart2(1)=i
        lstPart2(2)=j
        val newlst = genIntcode(lstPart2)
        if(newlst(0) == 19690720) {
          println(newlst.mkString(","))
          println(s"${newlst(0)} :: ${newlst(1)} :: ${newlst(2)}")
        }
      }
    }

    //19690720 :: 23 :: 47
  }

}
