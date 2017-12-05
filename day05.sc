import scala.annotation.tailrec
import scala.io.Source


def countSteps(instructions: List[Int]): Int = {
  jump(instructions, 0, 0)
}

@tailrec
def jump(instructions: List[Int], currentIndex: Int, count: Int): Int = {
  val currentInstruction = instructions(currentIndex)
  val nextIndex = currentInstruction + currentIndex

  if (nextIndex >= instructions.length) {
    return count + 1
  }

  jump(instructions.updated(currentIndex, currentInstruction + 1), nextIndex, count + 1)
}

def test() = {
  assert(countSteps(List(0, 3, 0, 1, -3)) == 5)
}

test()


val lines = Source.fromFile("day05-input.txt").getLines
val instr = lines.map(_.toInt).toList

println("Result: " + countSteps(instr))