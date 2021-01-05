package fr.efrei.fp

import fr.efrei.fp.model.{Airport, Country, Runway}

object Main extends App {
  println("Parsing CSV files...")

	Country.parseAllFromCSV("./src/resources/countries.csv") match {
    case Left(x) => println(x)
    case Right(countries) =>
      println("Done parsing and gathering parsable countries from countries.csv")
      countries.foreach(println)

      Airport.parseAllFromCSV("./src/resources/airports.csv") match {
        case Left(x) => println(x)
        case Right(airports) =>
          println("Done parsing and gathering parsable airports from airports.csv")
          airports.foreach(println)

          Runway.parseAllFromCSV("./src/resources/runways.csv") match {
            case Left(x) => println(x)
            case Right(runways) =>
              println("Done parsing and gathering parsable runways from runways.csv")
              runways.foreach(println)
          }
      }
  }
}