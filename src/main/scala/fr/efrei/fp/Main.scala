package fr.efrei.fp

import fr.efrei.fp.model.Country

object Main extends App {
	Country.parseAllFromCSV("./resources/countries.csv").foreach(println)
  println("truc")
}
