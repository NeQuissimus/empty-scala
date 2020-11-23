package model

case class Runway(
                   latitude: Double,
                   longitude: Double,
                   depart: String,
                   arrivee: String,
                   duree: Int)

  object Runway{
    def build(latitude: Double, longitude: Double, depart: String, arrivee: String, duree: Int): Option[Runway] = {
      if (depart == "test")
          Some(new Runway(latitude, longitude, depart, arrivee, duree))
      else if (arrivee == "test")
          Some(new Runway(latitude, longitude, depart, arrivee, duree))
      else
        None
    }
  }
