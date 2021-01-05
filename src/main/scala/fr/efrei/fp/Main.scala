package fr.efrei.fp

import fr.efrei.fp.model.util.MenuChoice
import fr.efrei.fp.model.{Airport, Country, Runway}

import java.util.Scanner
import scala.annotation.tailrec

object Main extends App {
  println("====================\nHello, ")

  println("Parsing CSV files...")

  println("Parsing countries...")
	parseValidCountries() match {
    case Left(x) => println(x)
    case Right(countries) =>
      println("Done parsing and gathering parsable countries from countries.csv")

      println("Parsing airports...")
      parseValidAirports() match {
        case Left(x) => println(x)
        case Right(airports) =>
          println("Done parsing and gathering parsable airports from airports.csv")

          println("Parsing runways")
          parseValidCountries() match {
            case Left(x) => println(x)
            case Right(runways) =>
              println("Done parsing and gathering parsable runways from runways.csv")

              println("Done parsing files")

              // Now we can actually do something

              getValidMenuChoice()
          }
      }
  }

  def parseValidCountries() = Country.parseAllFromCSV("./src/resources/countries.csv")
  def parseValidAirports() = Airport.parseAllFromCSV("./src/resources/airports.csv")
  def parseValidRunways() = Runway.parseAllFromCSV("./src/resources/runways.csv")

  def showMenu(): Unit = {
    println("===== What do you want to do? =====")
    println("1. Get the 10 countries with highest number of airports (with count)")
    println("2. Get the 10 countries with lowest number of airports (with count)")
    println("3. Get the type or runways surfaces per country")
    println("4. Get the 10 most common runway latitude")
    println("5. Do nothing and exit")
  }

  def askMenuChoice(): MenuChoice = {
    showMenu()

    val input = new Scanner(System.in).nextLine().replaceAll("\n", "").trim

    input match {
      case "1" => MenuChoice.COUNTRIES_WITH_MOST_AIRPORTS
      case "2" => MenuChoice.COUNTRIES_WITH_LESS_AIRPORTS
      case "3" => MenuChoice.TYPES_OF_RUNWAYS_PER_COUNTRY
      case "4" => MenuChoice.MOST_COMMON_RUNWAY_LATITUDES
      case "5" => MenuChoice.EXIT
      case _ => MenuChoice.INVALID_CHOICE
    }
  }

  @tailrec
  def getValidMenuChoice(): MenuChoice = {
    val choice = askMenuChoice()
    if (choice == MenuChoice.INVALID_CHOICE) {
      println("Invalid choice, please try again\n")
      getValidMenuChoice()
    } else {
      choice
    }
  }
}