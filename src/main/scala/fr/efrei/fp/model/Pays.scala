package fr.efrei.fp.model

case class Pays private( code: Int,
                         name: String)

object Pays{
	def build(code: Int, name:String): Pays = new Pays(code,name)
}

