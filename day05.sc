import scala.annotation.tailrec
import scala.io.Source


def countSteps1(instructions: List[Int]): Int = {
  jump1(instructions, 0, 0)
}

@tailrec
def jump1(instructions: List[Int], currentIndex: Int, count: Int): Int = {
  val currentInstruction = instructions(currentIndex)
  val nextIndex = currentInstruction + currentIndex

  if (nextIndex >= instructions.length) {
    return count + 1
  }

  jump1(instructions.updated(currentIndex, currentInstruction + 1), nextIndex, count + 1)
}

def test1() = {
  assert(countSteps1(List(0, 3, 0, 1, -3)) == 5)
}


@tailrec
def jump2(instructions: List[Int], currentIndex: Int, count: Int): Int = {
  val currentInstruction = instructions(currentIndex)
  val nextIndex = currentInstruction + currentIndex

  if (nextIndex >= instructions.length) {
    return count + 1
  }

  val offset = currentInstruction match {
    case x if x >= 3 => -1
    case x if x < 3 => 1
  }

  jump2(instructions.updated(currentIndex, currentInstruction + offset), nextIndex, count + 1)
}

def test2() = {
  assert(countSteps2(List(0, 3, 0, 1, -3)) == 10)
}

def countSteps2(instructions: List[Int]): Int = {
  jump2(instructions, 0, 0)
}



test1()

val lines = Source.fromFile("day05-input.txt").getLines
val instr = lines.map(_.toInt).toList

println("Result 1: " + countSteps1(instr))

test2()

println("Result 2: " + countSteps2(instr))
