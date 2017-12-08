import scala.io.Source

def get(map: scala.collection.mutable.Map[String, Int], value: String): Int = {
  if (!map.contains(value)) {
    map(value) = 0
  }

  map(value)
}


def maxRegister(input: String): Int = {
  val registers = scala.collection.mutable.Map[String, Int]()

  input.split('\n').foreach { instruction =>
    val (registerOperation, condition) = instruction.split(" if ") match {
      case Array(x, y) => (x, y)
    }

    val (conditionRegister, conditionOperation, conditionValue) = condition.split(' ') match {
      case Array(x, y, z) => (x, y, z.toInt)
    }

    val conditionRegisterValue: Int = get(registers, conditionRegister)

    val conditionFulfilled = conditionOperation match {
      case "==" => conditionRegisterValue == conditionValue
      case ">=" => conditionRegisterValue >= conditionValue
      case ">" => conditionRegisterValue > conditionValue
      case "!=" => conditionRegisterValue != conditionValue
      case "<" => conditionRegisterValue < conditionValue
      case "<=" => conditionRegisterValue <= conditionValue
    }

    if (conditionFulfilled) {
      val (register, operation, value) = registerOperation.split(' ') match {
        case Array(x, y, z) => (x, y, z.toInt)
      }

      val currentRegisterValue = get(registers, register)

      operation match {
        case "inc" => registers(register) += value
        case "dec" => registers(register) -= value
      }
    }
  }

  registers.maxBy(_._2)._2
}


def test(): Unit = {
  val testInput = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

  assert(maxRegister(testInput) == 1)
}

test()

val source = Source.fromFile("day08-input.txt").getLines().mkString("\n")

println("Result: " + maxRegister(source))