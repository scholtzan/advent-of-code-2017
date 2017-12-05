import scala.io.Source


def isValid1(passphrase: String): Boolean = {
  val words = passphrase.split(' ')
  words.distinct.length == words.length
}


def test1() = {
  assert(isValid1("aa bb cc dd ee"))
  assert(!isValid1("aa bb cc dd aa"))
  assert(isValid1("aa bb cc dd aaa"))
}


def isValid2(passphrase: String): Boolean = {
  val words = passphrase.split(' ')
  val wordsSorted = words.map(_.sorted)
  wordsSorted.distinct.length == wordsSorted.length
}


def test2() = {
  assert(isValid2("abcde fghij"))
  assert(!isValid2("abcde xyz ecdab"))
  assert(isValid2("a ab abc abd abf abj"))
  assert(isValid2("iiii oiii ooii oooi oooo"))
  assert(!isValid2("oiii ioii iioi iiio"))
}


test1()

val source = Source.fromFile("day04-input.txt")
println("Result1: " + source.getLines().count(isValid1))
source.close

test2()

val source2 = Source.fromFile("day04-input.txt")
println("Result2: " + source2.getLines().count(isValid2))
source2.close
