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
          parseValidRunways() match {
            case Left(x) => println(x)
            case Right(runways) =>
              println("Done parsing and gathering parsable runways from runways.csv")

              println("Done parsing files")

              // Now we can actually do something
              doWorkForAsLongAsNeeded(countries, airports, runways)
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
    println("4. Get the 10 most common runway latitudes")
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
  def validMenuChoice: MenuChoice = {
    val choice = askMenuChoice()
    if (choice == MenuChoice.INVALID_CHOICE) {
      println("Invalid choice, please try again\n")
      validMenuChoice
    } else {
      choice
    }
  }

  @tailrec
  def doWorkForAsLongAsNeeded(countries: Array[Country], airports: Array[Airport], runways: Array[Runway]): Unit = {
    validMenuChoice match {
      case MenuChoice.COUNTRIES_WITH_MOST_AIRPORTS =>
      case MenuChoice.COUNTRIES_WITH_LESS_AIRPORTS =>
      case MenuChoice.TYPES_OF_RUNWAYS_PER_COUNTRY =>
      case MenuChoice.MOST_COMMON_RUNWAY_LATITUDES =>
        println("The top 10 most common runway latitudes are:")
        top10MostCommonRunwayLatitudes(runways).zipWithIndex.foreach(r => println(s"${r._2 + 1} - ${r._1._1}deg (${r._1._2} occurences)"))
      case MenuChoice.EXIT => Unit
      case MenuChoice.INVALID_CHOICE => doWorkForAsLongAsNeeded(countries, airports, runways)
    }
  }

  def top10CountriesWithMostAirports(countries: Array[Country], airports: Array[Airport]): Array[(Country, Int)] = {
    ???
  }

  def top10CountriesWithLessAirports(countries: Array[Country], airports: Array[Airport]): Array[(Country, Int)] = {
    ???
  }

  def typesOfRunwaysPerCountry(countries: Array[Country], airports: Array[Airport], runways: Array[Runway]): Array[(Country, Array[String])] = {
    ???
  }

  def top10MostCommonRunwayLatitudes(runways: Array[Runway]): Array[(Float, Int)] = {
    runways.map(r => (r.le_latitude_deg, 1)).groupBy(_._1).map(x => (x._1, x._2.length)).toArray.sortBy(_._2).reverse.take(10)
  }
}