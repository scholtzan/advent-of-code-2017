import scala.annotation.tailrec
import scala.io.Source

def score(input: String): (Int, Int) = {
  scoreRec(input.toList, 0, 0, inGarbage = false, 0)
}

@tailrec
def scoreRec(input: List[Char], score: Int, depth: Int, inGarbage: Boolean, garbageCount: Int): (Int, Int) = {
  input match {
    case Nil => (score, garbageCount)
    case '!' :: _ :: tail => scoreRec(tail, score, depth, inGarbage, garbageCount)
    case '>' :: tail => scoreRec(tail, score, depth, inGarbage = false, garbageCount)
    case _ :: tail if inGarbage => scoreRec(tail, score, depth, inGarbage, garbageCount + 1)
    case '{' :: tail => scoreRec(tail, score, depth + 1, inGarbage, garbageCount)
    case '}' :: tail => scoreRec(tail, score + depth, depth - 1, inGarbage, garbageCount)
    case '<' :: tail => scoreRec(tail, score, depth, inGarbage = true, garbageCount)
    case _ :: tail => scoreRec(tail, score, depth, inGarbage, garbageCount)
  }
}


def test1(): Unit = {
  assert(score("{}")._1 == 1)
  assert(score("{{{}}}")._1 == 6)
  assert(score("{{},{}}")._1 == 5)
  assert(score("{{{},{},{{}}}}")._1 == 16)
  assert(score("{<a>,<a>,<a>,<a>}")._1 == 1)
  assert(score("{{<ab>},{<ab>},{<ab>},{<ab>}}")._1 == 9)
  assert(score("{{<!!>},{<!!>},{<!!>},{<!!>}}")._1 == 9)
  assert(score("{{<a!>},{<a!>},{<a!>},{<ab>}}")._1 == 3)
}



def test2(): Unit = {
  assert(score("<>")._2 == 0)
  assert(score("<random characters>")._2 == 17)
  assert(score("<<<<>")._2 == 3)
  assert(score("<{!>}>")._2 == 2)
  assert(score("<!!>")._2 == 0)
  assert(score("<!!!>>")._2 == 0)
  assert(score("{<{o\"i!a,<{i<a>")._2 == 10)
}

test1()
test2()

val source = Source.fromFile("day09-input.txt").getLines().mkString

println("Result: " + score(source))