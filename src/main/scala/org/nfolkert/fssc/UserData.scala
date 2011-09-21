package org.nfolkert.fssc

import model.{UserVenueHistoryEntry, UserVenueHistoryEntry2, UserVenueHistory, User}
import org.scalafoursquare.call.{HttpCaller, AuthApp}
import net.liftweb.util.Props
import org.joda.time.DateTime
import org.scalafoursquare.auth.OAuthFlow
import org.scalafoursquare.response.{Response, VenueExploreResponse, VenueCompact, VenueLocation, CheckinForFriend}
import org.nfolkert.lib.{T, Util}
import net.liftweb.common.{Full, Loggable, Box}

object UserData extends Loggable {
  val AUTH_TOKEN = Props.get("access.token.user").open_!
  val CLIENT_ID = Props.get("consumer.key").open_!
  val CLIENT_SECRET = Props.get("consumer.secret").open_!
  val CLIENT_CALLBACK = Props.get("consumer.callback.url").open_!

  def oauth = {
    new OAuthFlow(CLIENT_ID, CLIENT_SECRET, CLIENT_CALLBACK)
  }

  def getApp(token: String) = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000), token)


  def getUser(token: String): Option[User] = {
    val app = getApp(token)
    app.self.get.response.map(resp=>{
      User.findOrCreate(resp.user)
    })
  }

  def getVisitedPoints(token: String, user: User): Set[DataPoint[VisitData]] = {
    if (token == "test")
      sampleVisits
    else
      getPointsFromVenueHistory(token, user)
    // getPointsFromCheckinHistory(token)
  }

  def getRecommendedPoints(lat: Double,
                           lng: Double,
                           filters: Set[Rectangle],
                           recType: String,
                           token: String): Set[DataPoint[RecData]] = T("Get Recommendations") {
    if (recType == "none")
      Set[DataPoint[RecData]]()
    else {
      val app = getApp(token)
      val section = recType match {
        case "all" => None;
        case t => Some(t)
      }

      val raw = app.multi(
        app.exploreVenues(lat, lng, radius=Some(200), novelty=Some("new"), section=section),
        //app.exploreVenues(lat, lng, radius=Some(400), novelty=Some("new")),
        app.exploreVenues(lat, lng, radius=Some(750), novelty=Some("new"), section=section),
        //app.exploreVenues(lat, lng, radius=Some(1000), novelty=Some("new")),
        app.exploreVenues(lat, lng, radius=Some(5000), novelty=Some("new"), section=section)
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

  def getCachedUserVenueHistory(userid: String): Box[UserVenueHistory] = {
    T("Mongo UserVenueHistory") {
      UserVenueHistory.find(userid)
    }
  }

  def updateUserVenueHistory(token: String, userid: String, history: UserVenueHistory): Box[UserVenueHistory] = T("Update UserVenueHistory") {
    val lastUpdateSeconds = history.lastUpdate.value
    val lastRefreshSeconds = history.lastRefresh.value

    // If no updates have been made in a month, or last full rebuild is older than 6 months, do complete rebuild
    if (Util.dateFromSeconds(lastUpdateSeconds).isBefore(new DateTime().minusDays(30)) ||
        Util.dateFromSeconds(lastRefreshSeconds).isBefore(new DateTime().minusDays(180)))
      fetchUserVenueHistory(userid, token)
    else if (Util.dateFromSeconds(lastUpdateSeconds).isAfter(new DateTime().minusMinutes(1)))
      Full(history) // We can give a little delay on reloading?
    else {
      val app = getApp(token)
      val recent = app.selfVenueHistory(afterTimestamp = Some(lastUpdateSeconds)).get
      recent.response.map(r => {
        val newVenues = UserVenueHistory.userHistoryList(r)

        // TODO: if add but no merge: just push add on end of list, update the timestamp
        // TODO: if merge (with or without add): rebuild the whole list, update the timestamp
        // TODO: if no merge or add, just update the timestamp

        if (!newVenues.isEmpty) {
          val old = history.venues3.value.map(e=>(e.venueId, e)).toMap
          val merge = newVenues.filter(e => old.contains(e.venueId)).map(e=>(e.venueId, e)).toMap
          val add = newVenues.filterNot(e => old.contains(e.venueId))

          val out = old.toList.map(p => {
            val id = p._1
            val orig = p._2
            merge.get(id).map(m => {
              UserVenueHistoryEntry(m.venueId, m.name, m.lat, m.lng,
                orig.beenHere + m.beenHere, m.address, m.city, m.state, m.country, m.catId)
            }).getOrElse(orig)
          }) ++ add
          history.venues3(out)
        }
        history.setLastUpdate(new DateTime()).save

      })
    }
  }

  def fetchUserVenueHistory(userid: String, token: String): Box[UserVenueHistory] = {
    T("API UserVenueHistory") {
      val app = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000), token)
      val venueHistory = T("API Call") { app.selfVenueHistory().get }
      venueHistory.response.map(r => {
        T("Cache Save") {
          UserVenueHistory.create(userid, r)
        }
      })
    }
  }

  def getPointsFromVenueHistory(token: String, user: User) = {
    val cachedHistory: Box[UserVenueHistory] = getCachedUserVenueHistory(user.id.value)
    val history: Box[UserVenueHistory] =
      if (!cachedHistory.isEmpty)
        updateUserVenueHistory(token, user.id.value, cachedHistory.open_!)
      else
        fetchUserVenueHistory(user.id.value, token)

    T("Build Data Points") {
      history.map(r=>{
        /*
        r.venues2.value.map(i => {
          val v: UserVenueHistoryEntry2 = i
          val name = v.city.valueBox.or(v.state.valueBox.or(v.country.valueBox)).openOr(i.lat + ", " + i.lng)
          val data = VisitData(v.beenHere.value, name)
          DataPoint(v.lat.value, v.lng.value, Some(data))
        }).toSet
         */

        r.venues3.value.map(i => {
          val v: UserVenueHistoryEntry = i
          val name = v.city.orElse(v.state.orElse(v.country)).getOrElse(v.lat + ", " + v.lng)
          val data = VisitData(v.beenHere, name)
          DataPoint(v.lat, v.lng, Some(data))
        }).toSet
    }).openOr(Set[DataPoint[VisitData]]())
    }
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