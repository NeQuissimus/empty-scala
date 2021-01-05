package fr.efrei.fp

import fr.efrei.fp.model.{Airport, Country, Runway}

object Main extends App {
	Country.parseAllFromCSV("./src/resources/countries.csv") match {
    case Left(x) => println(x)
    case Right(countries) => countries.foreach(println)
  }

  Airport.parseAllFromCSV("./src/resources/airports.csv") match {
    case Left(x) => println(x)
    case Right(airports) => airports.foreach(println)
  }

  Runway.parseAllFromCSV("./src/resources/runways.csv") match {
    case Left(x) => println(x)
    case Right(runways) => runways.foreach(println)
  }
}