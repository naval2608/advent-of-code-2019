package day5

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Instruction(opCode: Int, parameter: Option[Seq[Int]])

object day5 {

  val validOpcodeExcept99 = List(1,2,3,4,7,8,5,6)
  var keepGoing = true
  var currentIndexTracker: Int = 0

  def readInstruction(uncodedInstruction: Int): Instruction = {
    print(s"uncodedInstruction: $uncodedInstruction")
    uncodedInstruction match {
      case 99 => new Instruction(99, None)
      case other if validOpcodeExcept99.contains(other%100) =>
        val opcode = other%100
        new Instruction( opcode, {

          if(other < 100 && other > 0) {
            opcode match {
              case (1|2|7|8) => Some(Seq(0,0,0))
              case (3|4) => Some(Seq(0))
              case (5|6) => Some(Seq(0,0))
              case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
                s"at index " +
                s"$currentIndexTracker")
            }
          } else if(other < 1000 && other >= 100) {
            val param1 = (other / 100) % 10
            assert(List(0,1).contains(param1))
            opcode match {
              case (1|2|7|8) => Some(Seq(param1, 0, 0))
              case (3|4) => Some(Seq(param1))
              case (5|6) => Some(Seq(param1,0))
              case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
                s"at index $currentIndexTracker")
            }
          } else if(other < 10000 && other >= 1000) {
            val param1 = (other / 100) % 10
            val param2 = (other / 1000) % 10
            assert(List(0,1).contains(param1))
            assert(List(0,1).contains(param2))
            assert(List(1,2,5,6,7,8).contains(opcode))
            opcode match {
              case (1|2|7|8) => Some(Seq(param1, param2, 0))
              case (5|6) => Some(Seq(param1, param2))
              case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
                s"at index $currentIndexTracker")
            }
          } else if(other < 100000 && other >= 10000) {
            val param1 = (other / 100) % 10
            val param2 = (other / 1000) % 10
            val param3 = (other / 10000) % 10
            assert(List(0,1).contains(param1))
            assert(List(0,1).contains(param2))
            assert(List(0,1).contains(param3))
            assert(List(1,2,7,8).contains(opcode))
            opcode match {
              case (1|2|7|8)  => Some(Seq(param1, param2, param3))
              case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
                s"at index $currentIndexTracker")
            }
          } else
            throw new Exception(s"$uncodedInstruction is not a valid instruction" +
              s"at index $currentIndexTracker")
        }
        )
      case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
        s"at index $currentIndexTracker")
    }
  }

  def printInstruction(ins: Instruction): Unit = {
    println(s" => Opcode: ${ins.opCode}, params: [${ins.parameter}]")
  }

  def genIntcode(lst: ListBuffer[Int]): ListBuffer[Int] = {

    def readInput(param: Int, parameterMode: Int): Int = {
      parameterMode match {
        case 0 => lst(param)
        case 1 => param
      }
    }

    def writeOutput(param: Int, parameterMode: Int, value: Int): Unit = {
      parameterMode match {
        case 0 => lst(param) = value
        case 1 => throw new Exception("instruction writes to should never be in immediate mode.")
      }
    }

    def doOperationAndUpdateCurrentIndex(ins: Instruction): Unit = {
      val paramSeq = ins.parameter.getOrElse(Seq())
      ins.opCode match {
        case it@(1|2) => {
          //instruction is current index rn.
          val input1 = readInput(lst(currentIndexTracker + 1), paramSeq(0))
          val input2 = readInput(lst(currentIndexTracker + 2), paramSeq(1))
          println(s"input1 $input1, input2 $input2")
          it match {
            case 1 => writeOutput(lst(currentIndexTracker + 3), paramSeq(2), input1 + input2)
            case 2 => writeOutput(lst(currentIndexTracker + 3), paramSeq(2), input1 * input2)
          }
          currentIndexTracker += 4
        }
        case 99 => {
          keepGoing = false
          currentIndexTracker += 1
          println("HALT::" + lst.mkString(","))
        }
        case 3 => {
          println("Enter the input: ")
          val input = scala.io.StdIn.readLine().toInt
          writeOutput(lst(currentIndexTracker + 1), 0, input)
          currentIndexTracker += 2
        }
        case 4 => {
          val valueToprint = readInput(lst(currentIndexTracker + 1), paramSeq(0))
          println(s"PRINTING: ${valueToprint}")
          currentIndexTracker += 2
        }
        case 5 => {
          val input1 = readInput(lst(currentIndexTracker + 1), paramSeq(0))
          val input2 = readInput(lst(currentIndexTracker + 2), paramSeq(1))
          println(s"input1 $input1, input2 $input2")
          if(input1 != 0) currentIndexTracker=input2 else currentIndexTracker += 3
        }
        case 6 => {
          val input1 = readInput(lst(currentIndexTracker + 1), paramSeq(0))
          val input2 = readInput(lst(currentIndexTracker + 2), paramSeq(1))
          println(s"input1 $input1, input2 $input2")
          if(input1 == 0) currentIndexTracker=input2 else currentIndexTracker += 3
        }
        case 7 => {
          val input1 = readInput(lst(currentIndexTracker + 1), paramSeq(0))
          val input2 = readInput(lst(currentIndexTracker + 2), paramSeq(1))
          println(s"input1 $input1, input2 $input2")
          if(input1 < input2) writeOutput(lst(currentIndexTracker + 3), 0, 1)
          else writeOutput(lst(currentIndexTracker + 3), 0, 0)

          currentIndexTracker += 4
        }
        case 8 => {
          val input1 = readInput(lst(currentIndexTracker + 1), paramSeq(0))
          val input2 = readInput(lst(currentIndexTracker + 2), paramSeq(1))
          println(s"input1 $input1, input2 $input2")
          if(input1 == input2) writeOutput(lst(currentIndexTracker + 3), 0, 1)
          else writeOutput(lst(currentIndexTracker + 3), 0, 0)

          currentIndexTracker += 4
        }
      }
    }

    while (keepGoing) {
      val instruction = readInstruction(lst(currentIndexTracker))
      printInstruction(instruction)
      doOperationAndUpdateCurrentIndex(instruction)
    }
    println(lst.mkString(","))
    //reset for next int code
    currentIndexTracker = 0
    keepGoing = true
    lst
  }

  def main(args: Array[String]): Unit = {

//    printInstruction(readInstruction(1002))
//    printInstruction(readInstruction(2))
//    printInstruction(readInstruction(1))
//    printInstruction(readInstruction(3))
//    printInstruction(readInstruction(4))
//    printInstruction(readInstruction(103))
//    printInstruction(readInstruction(99))
//    printInstruction(readInstruction(11002))
//    printInstruction(readInstruction(31002))

//    genIntcode(ListBuffer(1002,4,3,4,33))
//    println("==========")
//    genIntcode(ListBuffer(3,0,4,0,99))
//    genIntcode(ListBuffer(1101,100,-1,4,0))
    //genIntcode(ListBuffer(3,9,8,9,10,9,4,9,99,-1,8))
    //genIntcode(ListBuffer(3,9,7,9,10,9,4,9,99,-1,8))
    //genIntcode(ListBuffer(3,3,1108,-1,8,3,4,3,99))
    //genIntcode(ListBuffer(3,3,1107,-1,8,3,4,3,99))
    //genIntcode(ListBuffer(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9))
    //genIntcode(ListBuffer(3,3,1105,-1,9,1101,0,0,12,4,12,99,1))
//    genIntcode(ListBuffer(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
//      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
//      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99))

    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day5/input.txt"
    val lst = Source.fromFile(file).getLines().toList(0).split(",").map(_.toInt).to[ListBuffer]
    genIntcode(lst)

  }
}
