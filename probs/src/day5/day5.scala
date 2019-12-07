package day5

import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Instruction(opCode: Int, parameter: Option[Seq[Int]])

object day5 {

  val validOpcodeExcept99 = List(1,2,3,4)
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
              case it if 1 to 2 contains it => Some(Seq(0,0,0))
              case it if 3 to 4 contains it => Some(Seq(0))
              case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
                s"at index $currentIndexTracker")
            }
          } else if(other < 1000 && other >= 100) {
            val param1 = (other / 100) % 10
            assert(List(0,1).contains(param1))
            opcode match {
              case it if 1 to 2 contains it => Some(Seq(param1, 0, 0))
              case it if 3 to 4 contains it => Some(Seq(param1))
              case _ => throw new Exception(s"$uncodedInstruction is not a valid instruction" +
                s"at index $currentIndexTracker")
            }
          } else if(other < 10000 && other >= 1000) {
            val param1 = (other / 100) % 10
            val param2 = (other / 1000) % 10
            assert(List(0,1).contains(param1))
            assert(List(0,1).contains(param2))
            assert(List(1,2).contains(opcode))
            opcode match {
              case it if 1 to 2 contains it => Some(Seq(param1, param2, 0))
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
            assert(List(1,2).contains(opcode))
            opcode match {
              case it if 1 to 2 contains it  => Some(Seq(param1, param2, param3))
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
        case it if 1 to 2 contains it => {
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
    val file = "/Users/naval.gupta/code/advent-of-code-2019/probs/src/day5/input.txt"
    val lst = Source.fromFile(file).getLines().toList(0).split(",").map(_.toInt).to[ListBuffer]
    genIntcode(lst)
  }
}
