package fr.efrei.fp.model

sealed trait CountryCode
object CountryCode {
	private val countryCodeRegex = """^[A-Z]{2}$""".r

	def parse(code: String): Either[String, CountryCode] = code match {
		case countryCodeRegex(_*) => Right(CountryCodeImpl(code))
		case _ => Left(s"\"$code\" is not a valid country code")
	}

	private case class CountryCodeImpl(code: String) extends CountryCode
}
