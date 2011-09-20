package org.nfolkert.fssc.model

import net.liftweb.mongodb.{MongoDB, DefaultMongoIdentifier}
import net.liftweb.util.Props
import com.mongodb.{MongoOptions, ServerAddress, Mongo}

object MongoSetup {
  def init {
    val dbName = Props.get("mongo.db.name").open_!
    val dbAddress = Props.get("mongo.db.address").open_!
    val dbPort = Props.get("mongo.db.port").open_!.toInt

    val server = new ServerAddress(dbAddress, dbPort)
    val options = new MongoOptions
    MongoDB.defineDb(DefaultMongoIdentifier, new Mongo(server, options), dbName)
  }
}
