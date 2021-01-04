package fr.efrei.fp

import fr.efrei.fp.model.Country

object Main extends App {
	Country.parseAllFromCSV("./src/resources/countries.csv") match {
    case Left(x) => println(x)
    case Right(countries) => countries.foreach(println)
  }
}