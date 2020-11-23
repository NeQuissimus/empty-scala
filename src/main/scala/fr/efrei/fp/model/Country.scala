package fr.efrei.fp.model

import fr.efrei.fp.util.Digit

case class Country(id: Int, code: String, name: String, wikipedia_link: String, keywords: List[String])

object Country {
  def buildFrom(csvLine: String): Either[String, Country] = csvLine match {
      case Nil => Left("Cannot read Nil line")
      case "" => Left("Not enough data on CSV line to represent an airport")
      case _ => {
        val csvBits = csvLine.split(",").map(
          _.trim
        )

        csvBits.length match {
          case 6 => {
            // Parse id and ensure it is only made of digits
            csvBits(0).toCharArray.map(Digit.buildFrom)
          }
        }
      }
    }
}
