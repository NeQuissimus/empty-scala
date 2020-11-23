package fr.efrei.fp.util

sealed trait Digit
object Digit {
  case object `0` extends Digit
  case object `1` extends Digit
  case object `2` extends Digit
  case object `3` extends Digit
  case object `4` extends Digit
  case object `5` extends Digit
  case object `6` extends Digit
  case object `7` extends Digit
  case object `8` extends Digit
  case object `9` extends Digit

  def buildFrom(c: Char): Either[String, Digit] = c match {
    case '0' => Right(Digit.`0`)
    case '1' => Right(Digit.`1`)
    case '2' => Right(Digit.`2`)
    case '3' => Right(Digit.`3`)
    case '4' => Right(Digit.`4`)
    case '5' => Right(Digit.`5`)
    case '6' => Right(Digit.`6`)
    case '7' => Right(Digit.`7`)
    case '8' => Right(Digit.`8`)
    case '9' => Right(Digit.`9`)
    case _ => Left("Unable to parse digit")
  }
}
