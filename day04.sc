import scala.io.Source

val source = Source.fromFile("day04-input.txt")


def isValid(passphrase: String): Boolean = {
  val words = passphrase.split(' ')
  words.distinct.length == words.length
}


val validPassphrases = source.getLines().count(isValid)

source.close

println("Result: " + validPassphrases)