package day4

import util.control.Breaks._

object day4 {

  def passwordCriteriaMet(pwd: Int): Boolean = {
    var metCriteria = true
    println(s"\n\nanalysing $pwd")
    val arr = pwd.toString.toStream.toList.map( chr => chr.toString.toInt)
    //scala> 122345.toString.toStream.toList.map( chr => chr.toString.toInt)
    //res19: List[Int] = List(1, 2, 2, 3, 4, 5)


    breakable {

      if(arr.zipWithIndex.groupBy(_._1).filter(_._2.size > 1).size < 1) {
        metCriteria = false
        println(s"no digits repeats itself in $pwd")
        break
      }
      println(s"digits repeats itself in $pwd")

      val repeatingDigit =  arr.zipWithIndex.groupBy(_._1).filter(_._2.size > 1)
      var doubleGrouping = false
      //var minimumGroupSize = 0
      repeatingDigit.foreach{
        case (key,value) =>
          //println(s"digit: $key, index: $value")
          val indexList = value.map(_._2)
          println(s"digit: $key, index list -> $indexList, index list size -> ${indexList.size}")
          if(indexList.size == 2) {
            println(s"digit: $key, has double grouping")
            doubleGrouping = true
          }

          for(i <- 1 to indexList.size - 1) {
            if(indexList(i) - indexList(i-1) != 1) {
              //println(s"${indexList(i)}, ${indexList(i - 1)}")
              metCriteria = false
              println(s"digit: $key doesn't have repeating digit next to each other, indexList: " +
                s"${indexList.mkString(",")}")
              break
            }
          }
          println(s"repeating digits are adjacent for digit: $key")
      }

      if(!doubleGrouping) {
        metCriteria = false
        println(s"pwd: $pwd doesn't have double grouping")
        break
      }

      for (i <- 1 to arr.size - 1) {
        //println(s"${arr(i)}, ${arr(i - 1)}")
        if (arr(i) < arr(i - 1)) {
          metCriteria = false
          println(s"pwd didn't meet criteria, ${arr(i - 1)} is greater than ${arr(i)}")
          break
        }
      }
      println(s"digits never decrease in $pwd")
    }
    println(s"$pwd -> $metCriteria")
    metCriteria
  }

  def main(args: Array[String]): Unit = {
    var passwordStatisfyCounter = 0
    passwordCriteriaMet(112233)
    passwordCriteriaMet(123444)
    passwordCriteriaMet(111122)
    passwordCriteriaMet(223450)
    passwordCriteriaMet(111123)
    passwordCriteriaMet(135679)
    passwordCriteriaMet(122345)
    passwordCriteriaMet(111111)
    passwordCriteriaMet(123245)
    for(i <- 123257 to 647015) {
      if(passwordCriteriaMet(i)) passwordStatisfyCounter += 1
    }
    println(s"matching pwds are $passwordStatisfyCounter")
  }

}
