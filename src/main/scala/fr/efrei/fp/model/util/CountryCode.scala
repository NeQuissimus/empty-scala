package fr.efrei.fp.model.util

sealed trait CountryCode

object CountryCode {
  private val countryCodeRegex = "^[A-Z]{2}$"

  def parse(code: String): Either[String, CountryCode] = if (code.matches(countryCodeRegex)) {
    Right(CountryCodeImpl(code))
  } else {
    if (code.matches("^\"[A-Z]{2}\"$")) {
      Right(CountryCodeImpl(code.slice(1, 3)))
    } else {
      Left("\"" + code + "\" is not a valid country code")
    }
  }

  private case class CountryCodeImpl(code: String) extends CountryCode

}