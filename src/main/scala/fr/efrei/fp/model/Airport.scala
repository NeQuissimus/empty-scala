package fr.efrei.fp.model

case class Airport( name: String,
	                  pays: String,
	                  ville:String,
	                  destination:List[String])

object Airport {
	def build(name: String, pays: String, ville:String, destination:List[String]): Option[Airport] =
		if (pays=="test") Some(new Airport(name,pays,ville,destination))
		else None
}
