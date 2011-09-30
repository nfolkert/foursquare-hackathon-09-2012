package org.nfolkert.fssc

import model.{VenueCategories, UserVenueHistoryEntry, UserVenueHistory, User}
import net.liftweb.common.{Full, Loggable, Box}
import net.liftweb.json.JsonDSL._
import net.liftweb.util.Props
import org.joda.time.DateTime
import org.nfolkert.lib.{T, Util}
import org.nfolkert.snippet.Session
import org.scalafoursquare.auth.OAuthFlow
import org.scalafoursquare.call.{UserlessApp, HttpCaller, AuthApp}
import org.scalafoursquare.response.{VenueSearchResponse, UserCompact, Response, VenueExploreResponse, VenueCompact, VenueLocation, CheckinForFriend}

object UserData extends Loggable {
  val AUTH_TOKEN = Props.get("access.token.user").open_!
  val CLIENT_ID = Props.get("consumer.key").open_!
  val CLIENT_SECRET = Props.get("consumer.secret").open_!
  val CLIENT_CALLBACK = Props.get("consumer.callback.url").open_!

  def oauth = {
    new OAuthFlow(CLIENT_ID, CLIENT_SECRET, CLIENT_CALLBACK)
  }

  def getApp(token: String) = new AuthApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000), token)
  def getUserlessApp = new UserlessApp(HttpCaller(CLIENT_ID, CLIENT_SECRET, readTimeout=10000))

  def getUser(token: String): Option[User] = {
    val app = getApp(token)
    app.self.get.response.map(resp=>{
      User.findOrCreate(resp.user)
    })
  }

  def getUserFriends(token: String): List[UserCompact] = {
    val app = getApp(token)

    // TODO: should handle pages, followers
    val allFriends = app.multi(List(
      app.selfFriends(Some(500), Some(0)),
      app.selfFriends(Some(500), Some(500))
    )).get.responses.map(_.flatMap(_.response.map(_.friends.items).getOrElse(Nil))).getOrElse(Nil)

    val dbUsers = User.findAll(("_id" -> ("$in" -> allFriends.map(_.id))))
    val haveIds = dbUsers.map(_.id.value).toSet
    allFriends.filter(u=>haveIds.contains(u.id)).sortBy(u=>u.firstName+u.lastName.map(ln=>" "+ln).getOrElse(""))
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
                           grid: MapGrid,
                           filters: Set[Rectangle],
                           recType: String,
                           wide: Boolean,
                           token: String): Set[DataPoint[RecData]] = T("Get Recommendations") {
    if (recType == "none")
      Set[DataPoint[RecData]]()
    else {
      val app = getApp(token)
      val section = recType match {
        case "all" => None;
        case t => Some(t)
      }
      val gridSize = grid.latGridSizeInMeters

      val reqs = List(
          app.exploreVenues(lat, lng, radius=Some(gridSize), novelty=Some("new"), section=section),
          app.exploreVenues(lat, lng, radius=Some(gridSize*3), novelty=Some("new"), section=section)
        ) ++ (if (wide) List(app.exploreVenues(lat, lng, radius=Some(gridSize*10), novelty=Some("new"), section=section)) else Nil)

      val raw = app.multi(
        app.exploreVenues(lat, lng, radius=Some(gridSize), novelty=Some("new"), section=section),
        app.exploreVenues(lat, lng, radius=Some(gridSize*4), novelty=Some("new"), section=section),
        app.venueSearch(lat, lng, limit=Some(50), intent=Some("checkin")),
        app.venueSearch(lat, lng, limit=Some(50), intent=Some("match"))
      ).get

      // TODO: consider parallelizing locally; multi endpoint does not parallelize
      def filterRecommendations(response: Response[VenueExploreResponse]): List[DataPoint[RecData]] = {
        response.response.flatMap(_.groups.find(_.`type` == "recommended")).map(veg=>{
          val venues: List[VenueCompact] = veg.items.map(_.venue)
          val points: List[DataPoint[RecData]] = venues.flatMap(v=>for{lat <- v.location.lat; lng <- v.location.lng} yield DataPoint(lat, lng, Some(RecData(v))))
          points.filter(pt=>filters.find(_.contains(pt)).isDefined)
        }).getOrElse(Nil)
      }

      def filterSearch(response: Response[VenueSearchResponse]): List[DataPoint[RecData]] = {
        response.response.map(r=>{
          val venues: List[VenueCompact] = r.venues.filter(!_.categories.isEmpty)
          val points: List[DataPoint[RecData]] = venues.flatMap(v=>for{lat <- v.location.lat; lng <- v.location.lng} yield DataPoint(lat, lng, Some(RecData(v))))
          points.filter(pt=>filters.find(_.contains(pt)).isDefined)
        }).getOrElse(Nil)
      }

      val e1 = raw.responses._1.toList.flatMap(r=>filterRecommendations(r))
      val e2 = raw.responses._2.toList.flatMap(r=>filterRecommendations(r))
      val s1 = raw.responses._3.toList.flatMap(r=>filterSearch(r))
      val s2 = raw.responses._4.toList.flatMap(r=>filterSearch(r))

      val filtered = (e1 ++ e2 ++ s1 ++ s2).distinct
      val maxPerCell = 2
      grid.mapToGrid(filtered).toList.flatMap(p=>{
        val inGrid = p._2
        inGrid.take(maxPerCell) // Take the top recommedations for each grid cell
      }).toSet
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

  val MAX_IDLE_DAYS_TO_REFRESH = 7
  val MAX_DAYS_TO_REFRESH = 30

  def updateUserVenueHistory(token: String, userid: String, history: UserVenueHistory): Box[UserVenueHistory] = T("Update UserVenueHistory") {
    val lastUpdateSeconds = history.lastUpdate.value
    val lastRefreshSeconds = history.lastRefresh.value

    if (Util.dateFromSeconds(lastUpdateSeconds).isBefore(new DateTime().minusDays(MAX_IDLE_DAYS_TO_REFRESH)) ||
        Util.dateFromSeconds(lastRefreshSeconds).isBefore(new DateTime().minusDays(MAX_DAYS_TO_REFRESH)))
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
          val old = history.venues.value.map(e=>(e.venueId, e)).toMap
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
          history.venues(out)
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

    historyToVisitPoints(history)
  }

  def historyToVisitPoints(history: Box[UserVenueHistory]) = {
    T("Build Data Points") {
      history.map(r=>{
        r.venues.value.map(i => {
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

  lazy val venueCategories = {
    VenueCategories.create(getUserlessApp.venueCategories.get.response)
  }
  def initCaches {
    venueCategories
  }
}