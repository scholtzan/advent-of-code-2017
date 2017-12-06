import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack

val in = "4\t10\t4\t1\t8\t4\t9\t14\t5\t1\t14\t15\t0\t15\t3\t5"


def redistributionCycles(input: String): Int = {
  val memory = input.split('\t').map(_.toInt)
  val stack = mutable.Stack[String]()

  cycle(memory, stack)
}

@tailrec
def cycle(memory: Array[Int], stack: mutable.Stack[String]): Int = {
  val memorySize = memory.length
  val max = memory.max
  val maxIndex = memory.indexOf(max)

  val increases = max / memorySize
  val overlap = max % memorySize
  val overlapStart = maxIndex + overlap - memorySize + 1

  val updatedMemory = memory.zipWithIndex.map {
    case (value, index) =>
      if ((index > maxIndex && index <= maxIndex + overlap) ||
          overlapStart > 0 && index >= 0 && index < overlapStart) {
        value + increases + 1
      } else if (index == maxIndex) {
        increases
      } else {
        value + increases
      }
  }

  if (stack.contains(updatedMemory.mkString(" "))) {
    println("Result 2: " + stack.indexOf(updatedMemory.mkString(" ")) + 1)

    return stack.length + 1
  }

  stack.push(updatedMemory.mkString(" "))

  cycle(updatedMemory, stack)
}


def test1() = {
  val testInput = "0\t2\t7\t0"

  assert(redistributionCycles(testInput) == 5)
}

test1()

println("Result 1: " + redistributionCycles(in))
