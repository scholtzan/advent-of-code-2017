import scala.io.Source

def bottomProgram(input: String): String = {
  val lines = input.split('\n')

  // filter out leaves
  val programInnerNodesLines = lines.filter { l =>
    l.split("->").length > 1
  }

  val innerProgramNodes = programInnerNodesLines.flatMap { l =>
    val left = l.split(" -> ").head
    val right = l.split(" -> ").last
    left.split(" ").head +: (right.split(", ") ++ right.split(", "))
  }

  innerProgramNodes.find { p =>
    innerProgramNodes.count(_ == p) == 1
  }.get
}


def test() = {
  val testInput = "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

  assert(bottomProgram(testInput) == "tknk")
}

test()

val source = Source.fromFile("day07-input.txt").getLines().mkString("\n")
println("Result: " + bottomProgram(source))
