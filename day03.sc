def spiralMemory(input: Int): Int = {
  val spiral = math.ceil((-1 + math.sqrt(1 + 4 * 2 * math.ceil((input - 1) / 8.0))) / 2.0)
  val spiralMaxVal = 8 * spiral * (spiral + 1) / 2
  val dif = spiralMaxVal - input + 1

  (spiral + math.abs(spiral - (dif % (spiral * 2)))).toInt
}


def test() = {
  assert(spiralMemory(1) == 0)
  assert(spiralMemory(12) == 3)
  assert(spiralMemory(23) == 2)
  assert(spiralMemory(1024) == 31)
}


test()

println("Result: " + spiralMemory(312051))


// Part 2 by manual calculation using Excel: 312453