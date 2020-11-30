package fr.efrei.fp.model

import fr.efrei.fp.util.Digit
import java.net.URL
import scala.io.Source
import scala.util.Try

case class Country(id: Array[Digit], code: CountryCode, name: String, continent: Continent, wikipedia_link: URL, keywords: Array[String])

object Country {
	private val nonCountryCharRegex = """[_&~"\[\]|`\\/*µ$£¤<>,?;.:!§%²^\d]""".r

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

	private def parseName(name: String): Either[String, String] = name match {
		case nonCountryCharRegex(_*) => Right(name)
		case _ => Left(s"\"$name\" contains unauthorized characters")
	}

	def buildFromCSVLine(csvLine: String): Either[String, Country] = {
		val csvBits = csvLine.split(",").map(
			_.trim
		)

		csvBits.length match {
			case x if x < 6 => Left("Unable to parse incomplete data, 6 columns expected, only $x found")
			case _ =>
				// Parse id and ensure it is only made of digits
				val idEither = parseId(csvBits(0))
				val codeEither = CountryCode.parse(csvBits(1))
				val nameEither = parseName(csvBits(2))
				val continentEither = Continent.parse(csvBits(3))
				val wikiLinkEither = Try(new URL(csvBits(4))).toEither
				val keywords = csvBits.drop(5)

				(idEither, codeEither, nameEither, continentEither, wikiLinkEither) match {
					case (Left(x), _, _, _, _) => Left(x)
					case (_, Left(x), _, _, _) => Left(x)
					case (_, _, Left(x), _, _) => Left(x)
					case (_, _, _, Left(x), _) => Left(x)
					case (_, _, _, _, Left(_)) => Left("Unable to parse wikipedia link")
					case (Right(id), Right(code), Right(name), Right(continent), Right(wikiLink)) => Right(Country(id, code, name, continent, wikiLink, keywords))
					case _ => Left("Unable to parse country for some reason")
				}
		}
	}

  def parseAllFromCSV(csvFilePath: String): Either[String, Array[Country]] = {
    val csvFile = Source.fromFile(csvFilePath)
    val countries = csvFile.getLines.map(buildFromCSVLine)
    csvFile.close()

    if (countries.exists(_.isLeft)) {
      Left("Unable to parse some countries")
    } else {
      Right(countries.map(_.right.get).toArray)
    }
  }
}
