package org.nfolkert.fssc

import org.scalafoursquare.call.{HttpCaller, AuthApp}
import net.liftweb.util.Props
import org.joda.time.DateTime
import org.scalafoursquare.auth.OAuthFlow
import org.scalafoursquare.response.{Response, VenueExploreResponse, VenueCompact, VenueLocation, CheckinForFriend}

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

  def getRecommendedPoints(lat: Double, lng: Double, filters: Set[Rectangle], token: String): Set[DataPoint[RecData]] = {
    val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000), token)

    val raw = app.multi(
      app.exploreVenues(lat, lng, radius=Some(200)),
      //app.exploreVenues(lat, lng, radius=Some(400)),
      app.exploreVenues(lat, lng, radius=Some(750)),
      //app.exploreVenues(lat, lng, radius=Some(1000)),
      app.exploreVenues(lat, lng, radius=Some(5000))
    ).get

    def filterRecommendations(response: Option[Response[VenueExploreResponse]]) = {
      response.flatMap(_.response.flatMap(_.groups.find(_.`type` == "recommended")).map(veg=>{
        val venues: List[VenueCompact] = veg.items.map(_.venue)
        val points: List[DataPoint[RecData]] = venues.flatMap(v=>for{lat <- v.location.lat; lng <- v.location.lng} yield DataPoint(lat, lng, Some(RecData(v))))
        points.filter(pt=>filters.find(_.contains(pt)).isDefined)
          .map(p=>(p.distanceTo(lat, lng), p))
      })).getOrElse(Nil)
    }
    val close = filterRecommendations(raw.responses._1).sortBy(_._1).map(_._2)
    val mediumclose = Nil// filterRecommendations(raw.responses._2).sortBy(_._1).map(_._2)
    val medium = filterRecommendations(raw.responses._2).sortBy(_._1).map(_._2)
    val mediumfar = Nil// filterRecommendations(raw.responses._4).sortBy(_._1).map(_._2)
    val far = filterRecommendations(raw.responses._3).sortBy(_._1).map(_._2)

    (close.take(5) ++
     mediumclose.take(5) ++
     medium.take(5) ++
     mediumfar.take(5) ++
     far.take(5)).toSet
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