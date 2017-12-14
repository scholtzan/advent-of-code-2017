import scala.annotation.tailrec
import scala.io.Source

def score(input: String): Int = {
  scoreRec(input.toList, 0, 0, inGarbage = false)
}

@tailrec
def scoreRec(input: List[Char], score: Int, depth: Int, inGarbage: Boolean): Int = {
  input match {
    case Nil => score
    case '!' :: _ :: tail => scoreRec(tail, score, depth, inGarbage)
    case '>' :: tail => scoreRec(tail, score, depth, inGarbage = false)
    case _ :: tail if inGarbage => scoreRec(tail, score, depth, inGarbage)
    case '{' :: tail => scoreRec(tail, score, depth + 1, inGarbage)
    case '}' :: tail => scoreRec(tail, score + depth, depth - 1, inGarbage)
    case '<' :: tail => scoreRec(tail, score, depth, inGarbage = true)
    case _ :: tail => scoreRec(tail, score, depth, inGarbage)
  }
}


def test(): Unit = {
  assert(score("{}") == 1)
  assert(score("{{{}}}") == 6)
  assert(score("{{},{}}") == 5)
  assert(score("{{{},{},{{}}}}") == 16)
  assert(score("{<a>,<a>,<a>,<a>}") == 1)
  assert(score("{{<ab>},{<ab>},{<ab>},{<ab>}}") == 9)
  assert(score("{{<!!>},{<!!>},{<!!>},{<!!>}}") == 9)
  assert(score("{{<a!>},{<a!>},{<a!>},{<ab>}}") == 3)
}

test()

val source = Source.fromFile("day09-input.txt").getLines().mkString

println("Result: " + score(source))