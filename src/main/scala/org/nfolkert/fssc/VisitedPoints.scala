package org.nfolkert.fssc

import org.scalafoursquare.call.{HttpCaller, AuthApp}
import net.liftweb.util.Props
import org.joda.time.DateTime
import org.scalafoursquare.response.{VenueLocation, CheckinForFriend}
import org.scalafoursquare.auth.OAuthFlow

object VisitedPoints {
  val AUTH_TOKEN = Props.get("access.token.user").open_!
  val CLIENT_ID = Props.get("consumer.key").open_!
  val CLIENT_SECRET = Props.get("consumer.secret").open_!

  def getVisitedPoints: Set[VPt] = {
    // sampleData
    getPointsFromVenueHistory
    // getPointsFromCheckinHistory
  }

  def sampleData() = {
    Set(
      VPt(48.27, -101.28),
      VPt(25.82, -80.28),
      VPt(40.77, -73.98),
      VPt(40.77, -73.985),
      VPt(40.775, -73.98)
    )
  }

  def getPointsFromVenueHistory() = {
    val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET), AUTH_TOKEN)

    val venueHistory = app.selfVenueHistory().get
    venueHistory.response.map(r => {
      r.venues.items.flatMap(i => {
        val loc = i.venue.location
        for {lat <- loc.lat; lng <- loc.lng} yield {VPt(lat, lng, i.beenHere)}
      }).toSet
    }).getOrElse(Set[VPt]())
  }

  def getPointsFromCheckinHistory() = {
    val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET), AUTH_TOKEN)

    var baseOffset = 0
    var keepGoing = true;
    var checkinsList = List[CheckinForFriend]()

    while (keepGoing) {
      val reqs = (for (i <- 0 to 4) yield (app.selfCheckins(Some(250), Some(baseOffset + (250*i))))).toList
      val response = app.multi(reqs).get
      val checkins = response.responses.map(_.flatMap(_.response.map(_.checkins.items).getOrElse(Nil))).getOrElse(Nil)
      checkinsList = checkinsList ++ checkins
      if (checkins.length < (250*5))
        keepGoing = false
      else
        baseOffset = baseOffset + (250*5)
    }

    checkinsList.flatMap(c=>{
      c.venue.flatMap(venue => {
        val loc = venue.location
        for {lat <- loc.lat; lng <- loc.lng} yield {VPt(lat, lng, 1, Some(new DateTime(c.createdAt*1000L)))}
      })
    }).toSet
  }
}