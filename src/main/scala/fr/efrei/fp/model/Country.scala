package fr.efrei.fp.model

import fr.efrei.fp.util.Digit

import java.net.URL
import scala.util.Try


case class Country(id: Array[Digit], code: String, name: String, continent: Continent, wikipedia_link: URL, keywords: Array[String])

object Country {
  private def parseId(countryId: String): Either[String, Array[Digit]] = {
    val countryIdDigits = countryId.toCharArray.map(Digit.buildFrom)

    // Check if format is valid
    if (countryIdDigits.foldLeft(false) { (acc, id) =>
      id match {
        case x if acc || x.isLeft => true
        case _ => false
      }
    }) {
      Left(s"Cannot parse non-digit characters in \"$countryId\"")
    } else {
      Right(countryIdDigits.map(_.right.get))
    }
  }

  def buildFrom(csvLine: String): Either[String, Country] = {
    val csvBits = csvLine.split(",").map(
      _.trim
    )

    csvBits.length match {
      case x if x < 6 => Left("Unable to parse incomplete data, 6 columns expected, only $x found")
      case _ => {
        // Parse id and ensure it is only made of digits
        val idEither = parseId(csvBits(0))
        val countryCode = csvBits(1)
        val countryName = csvBits(2)
        val continentEither = Continent.parse(csvBits(3))
        val wikiLinkEither = Try(new URL(csvBits(4))).toEither
        val keywords = csvBits.drop(5)

        (idEither, continentEither, wikiLinkEither) match {
          case (Left(_), _, _) => Left("Unable to parse country ID")
          case (_, Left(_), _) => Left("Unable to parse continent")
          case (_, _, Left(_)) => Left("Unable to parse Wiki link")
          case (Right(id), Right(continent), Right(wikiLink)) => Right(Country(id, countryCode, countryName, continent, wikiLink, keywords))
          case _ => Left("Unable to parse country for some reason")
        }
      }
    }
  }
}
