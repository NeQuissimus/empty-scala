package fr.efrei.fp.model
import fr.efrei.fp.util.{Continent, CountryCode, Digit}
import java.net.URL
import scala.io.Source
import scala.util.Try
case class Airport( id: Array[Digit],
                    ident: String,
                    types: String,
                    name: String,
                    latitude_deg: float,
                    longitude_deg: float,
                    elevation_ft: int,
                    continent: Continent,
                    iso_country: CountryCode,
                    iso_region: String,
                    municipality: String,
                    scheduled_service: boolean,
                    gps_code: String,
                    iata_code: String,
                    lacal_code: String,
                    home_link: String,
                    wikipedia_link: String,
                    keywords: Array[String])

object Airport {
  private val nonCountryCharRegex = """[_&~"\[\]|`\\/*µ$£¤<>,?;.:!§%²^\d]""".r
  private val Identifiant= Set()
  private def parseId(airportId: String): Either[String, Array[Digit]] = {
		val airportIdDigits = airportId.toCharArray.map(Digit.buildFrom)

		// Check if format is valid
		if (airportIdDigits.foldLeft(false) { (acc, id) =>
			id match {
				case x if acc || x.isLeft => true
				case _ => false
			}
			}) {
			Left(s"""Cannot parse non-digit characters in "$airportId"""")
		} else {
			Right(airportIdDigits.map(_.right.get)
        if Identifiant(airportIdDigits){

        }
            Left("ID existant")
          else {
            Identifiant = Identifiant + airportIdDigits
          }
1		}
	}
  private def parseIdent(name: String): Either[String, String] = name match {
    case nonCountryCharRegex(_*) => Right(name)
    case _ => Left(s""""$name" contains unauthorized characters""")
  }
  private def parseIdent(name: String): Either[String, String] = name match {
    case nonCountryCharRegex(_*) => Right(name)
    case _ => Left(s""""$name" contains unauthorized characters""")
  }

  def buildFromCSVLineAirport(csvLine: String): Either[String, Airport] = {
    val csvBits = csvLine.split(",").map(
      _.trim
    )
    private def parseName(name: String): Either[String, String] = name match {
      case nonCountryCharRegex(_*) => Right(name)
      case _ => Left(s""""$name" contains unauthorized characters""")
    }
    private def parseLatitude(latitude: String): Either[String,Float] = {
      floatLatitude=latitude.toFloat
      floatLatitude match {
        case floatLatitude if floatLatitude < -90 => Left("Invalid Latitude")
        case floatLatitude if floatLatitude > 90 => Left("Invalid Latitude")
        case _ => Right(floatLatitude)
      }
    }
    private def parseLongitude(longitude: String): Either[String,Float] = {
      floatLongitude=longitude.toFloat
      floatLongitude match {
        case floatLongitude if floatLongitude < -180 => Left("Invalid Latitude")
        case floatLongitude if floatLongitude > 180 => Left("Invalid Latitude")
        case _ => Right(floatLongitude)
      }
    }
    private def parseElevation(elevation: String): Either[String,Int] = {
      Intelevation = elevation.toInt
      Intelevation match {
        case Intelevation if Intevaluation < 0 => Left("Invalid Elevation")
        case _ => Right (Intelevation)
      }
    }
    private def parseType(Type: String): Either[String,String]= Type match {
      case "heliport" => Right("heliport")
      case "small_airport"=> Right("small_airport")
      case "closed"=>Right("closed")
      case "seaplane_base"=>Right("sealplane_base")
      case "medium_airport"=>Right("medium_airport")
      case "large_airport"=>Right("large_airport")
      case Left("Invalid")
    }
    private def parseMunicipality(name: String): Either[String, String] = name match {
      case nonCountryCharRegex(_*) => Right(name)
      case _ => Left(s""""$name" contains unauthorized characters""")
    }
    private def parseSceduled(sceduled :String): boolean = sceduled match {
      case "yes" => true
      case "no"  => false
      case _=> null
    }
    def buildFromCSVLine2(csvLine: String): Either[String, Country] = {
      val csvBits = csvLine.split(",").map(
        _.trim
      )
    csvBits.length match {
      case x if x < 18 => Left(s"Unable to parse incomplete data, 18 columns expected, only $x found")
      case _ =>
        // Parse id and ensure it is only made of digits
        val idEither = parseId(csvBits(0))//ckeck
        val identEither = parseIdent(csvBits(1))//check
        val typesEither = parseType(csvBits(2))//check
        val nameEither = parseName(csvBits(3))//check
        val latitude_degEither = parseLatitude(csvBits(4))//check
        val longitidue_degEither = parseLongitude(csvBits(5))//check
        val elevation_ftEither = parseElevation(csvBits(6))//check
        val continentEither = Continent.parse(csvBits(7))//check
        val iso_country = CountryCode.parse(csvBits(8))//check
        val iso_region = parseIsoregion(csvBits(9))
        val municipality = parseMunicipality(csvBits(10))//check
        val scheduledEither = parseSceduled(csvBits(11))//check
        val gps_codeEither = parseIdent(csvBits(12))//check
        val iata_code = parseIdent(csvBits(13))//check
        val local_code = pparseIdent(csvBits(14))//check
        val home_link = Try(new URL(csvBits(15))).toEither//check
        val wikilinkEither = Try(new URL(csvBits(16))).toEither//check
        val keywords = csvBits.drop(17)//check

        (idEither, codeEither, nameEither, continentEither, wikiLinkEither) match {
          case (Left(x), _, _, _, _,_,_,_,_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_,Left(x), _, _, _,_,_,_,_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _,Left(x), _, _,_,_,_,_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _,Left(x), _,_,_,_,_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,Left(x),_,_,_,_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,Left(x),_,_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,Left(x),_,_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,_,Left(x),_,_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,_,_,Left(x),_,_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,_,_,_,Left(x),_,_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,_,_,_,_,Left(x),_,_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,_,_,_,_,_,Left(x),_,_,_,_,_) => Left(x)
          case (_, _, _, _,_,_,_,_,_,_,_,_,_,Left(x),_,_,_,_) => Left(x)
          case (Right(id), Right(code), Right(name), Right(continent), Right(wikiLink)) => Right(Country(id, code, name, continent, wikiLink, keywords))
          case _ => Left("Unable to parse airport for some reason")
        }
    }
  }
}
