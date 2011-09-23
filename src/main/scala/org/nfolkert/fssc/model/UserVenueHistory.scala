package org.nfolkert.fssc.model

import com.mongodb.{BasicDBList, DBObject, BasicDBObjectBuilder}
import net.liftweb.mongodb.record.{BsonMetaRecord, BsonRecord, MongoRecord, MongoMetaRecord}
import net.liftweb.mongodb.record.field.{MongoListField, BsonRecordListField}
import net.liftweb.common.{Full, Box}
import org.scalafoursquare.response.UserVenueHistoryResponse
import scala.collection.JavaConversions._
import net.liftweb.record.field.{LongField, DoubleField, IntField, StringField}
import org.nfolkert.lib.Util
import org.joda.time.DateTime

object UserVenueHistoryEntry {
  def deserialize(obj: DBObject): UserVenueHistoryEntry = {
    UserVenueHistoryEntry(
      obj.get("i").asInstanceOf[String],
      obj.get("n").asInstanceOf[String],
      obj.get("l").asInstanceOf[Number].doubleValue,
      obj.get("g").asInstanceOf[Number].doubleValue,
      obj.get("b").asInstanceOf[Number].intValue,
      Option(obj.get("a").asInstanceOf[String]),
      Option(obj.get("c").asInstanceOf[String]),
      Option(obj.get("s").asInstanceOf[String]),
      Option(obj.get("k").asInstanceOf[String]),
      Option(obj.get("y").asInstanceOf[String])
    )
  }
}

case class UserVenueHistoryEntry(venueId: String, name: String, lat: Double, lng: Double, beenHere: Int,
                                 address: Option[String], city: Option[String], state: Option[String],
                                 country: Option[String], catId: Option[String]) {
  def serialize: DBObject = {
    val o = new BasicDBObjectBuilder()
      .add("i", venueId)
      .add("n", name)
      .add("l", lat)
      .add("g", lng)
      .add("b", beenHere)
    address.map(v=>o.add("a", v))
    city.map(v=>o.add("c", v))
    state.map(v=>o.add("s", v))
    country.map(v=>o.add("k", v))
    catId.map(v=>o.add("y", v))
    o.get
  }
}

class UserVenueHistory extends MongoRecord[UserVenueHistory] {
  def meta = UserVenueHistory

  object id extends StringField(this, 100) {
    override def name = "_id"
  }

  object lastRefresh extends LongField(this) {
    override def name = "lr"
  }
  def getLastRefresh = Util.dateFromSeconds(lastRefresh.value)
  def setLastRefresh(date: DateTime) = lastRefresh(Util.secondsFromDate(date))

  object lastUpdate extends LongField(this) {
    override def name = "lu"
  }
  def getLastUpdate = Util.dateFromSeconds(lastUpdate.value)
  def setLastUpdate(date: DateTime) = lastUpdate(Util.secondsFromDate(date))

  object venues extends MongoListField[UserVenueHistory, UserVenueHistoryEntry](this) {
    override def name = "vs"
    override def asDBObject: DBObject = {
      val dbl = new BasicDBList
      val list: List[UserVenueHistoryEntry] = value
      list.map(e=>{
        dbl.add(e.serialize)
      })
      dbl
    }

    override def setFromDBObject(dbo: DBObject): Box[List[UserVenueHistoryEntry]] = {
      val dbList: List[AnyRef] = dbo.asInstanceOf[BasicDBList].toList
      val list: List[UserVenueHistoryEntry] = dbList.map(o=>UserVenueHistoryEntry.deserialize(o.asInstanceOf[DBObject]))
      setBox(Full(list))
    }
  }
}

object UserVenueHistory extends UserVenueHistory with MongoMetaRecord[UserVenueHistory] {
  override def collectionName = "user_venue_histories"

  def userHistoryList(uvh: UserVenueHistoryResponse): List[UserVenueHistoryEntry] = {
    uvh.venues.items.flatMap(e=>{
      val venueId = e.venue.id
      val venueName = e.venue.name
      val loc = e.venue.location
      val catId = e.venue.categories.find(_.primary.getOrElse(false)).flatMap(c=>c.id)

      val beenHere = e.beenHere
      loc.address

      for {
        lat <- loc.lat
        lng <- loc.lng
      } yield {
        UserVenueHistoryEntry(venueId, venueName, lat, lng, beenHere,
          loc.address, loc.city, loc.state, loc.country, catId)
      }
    })
  }

  def create(userid: String, uvh: UserVenueHistoryResponse): UserVenueHistory = {
    UserVenueHistory.createRecord.id(userid)
      .setLastUpdate(new DateTime)
      .setLastRefresh(new DateTime)
      .venues(userHistoryList(uvh))
    .save
  }
}
