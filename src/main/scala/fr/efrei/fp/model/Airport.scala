package fr.efrei.fp.model

import fr.efrei.fp.model.util.{Continent, CountryCode, Digit}

import java.net.URL
import scala.util.Try

case class Airport(id: Array[Digit],
                   ident: String,
                   types: String,
                   name: String,
                   latitude_deg: Float,
                   longitude_deg: Float,
                   elevation_ft: Int,
                   continent: Continent,
                   iso_country: CountryCode,
                   iso_region: String,
                   municipality: String,
                   scheduled_service: Boolean,
                   gps_code: String,
                   iata_code: String,
                   lacal_code: String,
                   home_link: String,
                   wikipedia_link: String,
                   keywords: Array[String])

object Airport {
  private val nonCountryCharRegex = """[_&~"\[\]|`\\/*µ$£¤<>,?;.:!§%²^\d]""".r

  def buildFromCSVLine2(csvLine: String): Either[String, Airport] = {
    val csvBits = csvLine.split(",").map(
      _.trim
    )

    csvBits.length match {
      case x if x < 18 => Left(s"Unable to parse incomplete data, 18 columns expected, only $x found")
      case _ =>
        // Parse id and ensure it is only made of digits
        val idEither = parseId(csvBits(0)) //ckeck
        val identEither = parseIdent(csvBits(1)) //check
        val typesEither = parseType(csvBits(2)) //check
        val nameEither = parseName(csvBits(3)) //check
        val latitude_degEither = parseLatitude(csvBits(4)) //check
        val longitidue_degEither = parseLongitude(csvBits(5)) //check
        val elevation_ftEither = parseElevation(csvBits(6)) //check
        val continentEither = Continent.parse(csvBits(7)) //check
        val iso_country = CountryCode.parse(csvBits(8)) //check
        val iso_region = csvBits(9)
        val municipality = parseMunicipality(csvBits(10)) //check
        val scheduledEither = parseScheduled(csvBits(11)) //check
        val gps_codeEither = parseIdent(csvBits(12)) //check
        val iata_code = parseIdent(csvBits(13)) //check
        val local_code = parseIdent(csvBits(14)) //check
        val home_link = Try(new URL(csvBits(15))).toEither //check
        val wikilinkEither = Try(new URL(csvBits(16))).toEither //check
        val keywords = csvBits.drop(17) //check

        if (Array(
          idEither,
          identEither,
          typesEither,
          nameEither,
          latitude_degEither,
          longitidue_degEither,
          elevation_ftEither,
          continentEither,
          iso_country,
          municipality,
          gps_codeEither,
          iata_code,
          local_code,
          home_link,
          wikilinkEither
        ).foldLeft(false) { (alreadyOneLeft, element) => alreadyOneLeft || element.isLeft }) {
          Left("Unable to parse Airport data from CSV line")
        } else {
          Right(Airport(idEither.right.get, identEither.right.get, typesEither.right.get, nameEither.right.get, latitude_degEither.right.get, longitidue_degEither.right.get, elevation_ftEither.right.get, continentEither.right.get, iso_country.right.get, iso_region, municipality.right.get, scheduledEither, gps_codeEither.right.get, iata_code.right.get, "", "", "", Array("")))
        }
    }
  }

  private def parseId(airportId: String): Either[String, Array[Digit]] = {
    val airportIdDigits = airportId.toCharArray.map(Digit.buildFrom)

    // Check if format is valid
    if (airportIdDigits.foldLeft(false) { (acc, id) =>
      id match {
        case x if acc || x.isLeft => true // Check
        case _ => false
      }
    }) {
      Left(s"""Cannot parse non-digit characters in "$airportId"""")
    } else {
      Right(airportIdDigits.map(_.right.get))
    }
  }

  private def parseIdent(name: String): Either[String, String] = name match {
    case nonCountryCharRegex(_*) => Right(name)
    case _ => Left(s""""$name" contains unauthorized characters""")
  }

  private def parseName(name: String): Either[String, String] = name match {
    case nonCountryCharRegex(_*) => Right(name)
    case _ => Left(s""""$name" contains unauthorized characters""")
  }

  private def parseLatitude(latitude: String): Either[String, Float] = {
    val floatLatitude = latitude.toFloat

    floatLatitude match {
      case floatLatitude if floatLatitude < -90 => Left("Invalid Latitude")
      case floatLatitude if floatLatitude > 90 => Left("Invalid Latitude")
      case _ => Right(floatLatitude)
    }
  }

  private def parseLongitude(longitude: String): Either[String, Float] = {
    val floatLongitude = longitude.toFloat

    floatLongitude match {
      case floatLongitude if floatLongitude < -180 => Left("Invalid Latitude")
      case floatLongitude if floatLongitude > 180 => Left("Invalid Latitude")
      case _ => Right(floatLongitude)
    }
  }

  private def parseElevation(elevation: String): Either[String, Int] = {
    val intElevation = elevation.toInt
    intElevation match {
      case intElevation if intElevation < 0 => Left("Invalid Elevation")
      case _ => Right(intElevation)
    }
  }

  private def parseType(Type: String): Either[String, String] = Type match {
    case "heliport" => Right("heliport")
    case "small_airport" => Right("small_airport")
    case "closed" => Right("closed")
    case "seaplane_base" => Right("sealplane_base")
    case "medium_airport" => Right("medium_airport")
    case "large_airport" => Right("large_airport")
    case _ => Left("Invalid")
  }

  private def parseMunicipality(name: String): Either[String, String] = name match {
    case nonCountryCharRegex(_*) => Right(name)
    case _ => Left(s""""$name" contains unauthorized characters""")
  }

  private def parseScheduled(scheduled: String): Boolean = scheduled match {
    case "yes" => true
    case _ => false
  }
}