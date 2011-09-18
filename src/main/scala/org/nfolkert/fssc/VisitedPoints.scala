package org.nfolkert.fssc

import org.scalafoursquare.call.{HttpCaller, AuthApp}
import net.liftweb.util.Props
import org.joda.time.DateTime
import org.scalafoursquare.auth.OAuthFlow
import org.scalafoursquare.response.{VenueCompact, VenueLocation, CheckinForFriend}

object VisitedPoints {
  val AUTH_TOKEN = Props.get("access.token.user").open_!
  val CLIENT_ID = Props.get("consumer.key").open_!
  val CLIENT_SECRET = Props.get("consumer.secret").open_!
  val CLIENT_CALLBACK = Props.get("consumer.callback.url").open_!

  def oauth = {
    new OAuthFlow(CLIENT_ID, CLIENT_SECRET, CLIENT_CALLBACK)
  }

  def getVisitedPoints(token: String): Set[DataPoint[VisitData]] = {
    if (token == "test")
      sampleVisits
    else
      getPointsFromVenueHistory(token)
    // getPointsFromCheckinHistory(token)
  }

  def getRecommendedPoints(center: DataPoint[VisitData], radius: Int, filters: Set[Rectangle], token: String): Set[DataPoint[RecData]] = {
    val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000), token)
    val recs: List[VenueCompact] =
      app.exploreVenues(center.lat, center.lng, radius=radius).get.response.flatMap(_.groups.find(_.`type` == "recommended")).map(_.items.map(_.venue)).getOrElse(Nil)
    val points: List[DataPoint[RecData]] = recs.flatMap(v=>for {lat <- v.location.lat; lng <- v.location.lng} yield DataPoint(lat, lng, RecData(v)))
    points.filter(pt=>filters.find(_.contains(pt)).isDefined)
  }

  def sampleRec() = {
    Set[DataPoint[RecData]]()
  }

  def sampleVisits() = {
    Set(
      DataPoint[VisitData](48.27, -101.28, Some(VisitData(18, "Minot"))),
      DataPoint[VisitData](25.82, -80.28, Some(VisitData(1, "Miami"))),
      DataPoint[VisitData](40.77, -73.98, Some(VisitData(2, "Brooklyn"))),
      DataPoint[VisitData](40.77, -73.985, Some(VisitData(1, "Brooklyn"))),
      DataPoint[VisitData](40.775, -73.98, Some(VisitData(1, "New York")))
    )
  }

  def getPointsFromVenueHistory(token: String) = {
    val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000), token)

    val venueHistory = app.selfVenueHistory().get
    venueHistory.response.map(r => {
      r.venues.items.flatMap(i => {
        val loc = i.venue.location
        for {lat <- loc.lat; lng <- loc.lng} yield {
          val name = loc.city.orElse(loc.state.orElse(loc.country)).getOrElse(lat + ", " + lng)
          DataPoint(lat, lng, Some(VisitData(i.beenHere, name)))
        }
      }).toSet
    }).getOrElse(Set[DataPoint[VisitData]]())
  }

  def getPointsFromCheckinHistory(token: String) = {
    val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET), token)

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
        for {lat <- loc.lat; lng <- loc.lng} yield {
          val name = loc.city.orElse(loc.state.orElse(loc.country)).getOrElse(lat + ", " + lng)
          DataPoint(lat, lng, Some(VisitData(1, name, new DateTime(c.createdAt*1000L))))
        }
      })
    }).toSet
  }
}