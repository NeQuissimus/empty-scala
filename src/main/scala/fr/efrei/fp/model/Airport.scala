package fr.efrei.fp.model

case class Airport( id: int,
                    ident: String,
                    types: String,
                    name: String,
                    latitude_deg: float,
                    longitude_deg: float,
                    elevation_ft: int,
                    continent: String,
                    iso_country: String,
                    iso_region: String,
                    municipality: String,
                    scheduled_service: boolean,
                    gps_code: String,
                    iata_code: String,
                    lacal_code: String,
                    home_link: String,
                    wikipedia_link: String,
                    keywords: String,
                  )

object Airport {
	def build(id: int,ident: String,types: String,name: String,latitude_deg: float,longitude_deg: float,elevation_ft: int,continent: String,iso_country: String,iso_region: String,
            municipality: String,scheduled_service: boolean,gps_code: String,iata_code: String,lacal_code: String,home_link: String,wikipedia_link: String,keywords: String,):
            Option[Airport] =
		if (id=="test") Some(new Airport(name,pays,ville,destination))
		else None
}
