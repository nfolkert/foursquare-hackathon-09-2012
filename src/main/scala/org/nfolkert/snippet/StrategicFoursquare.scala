package org.nfolkert.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.{SessionVar, SHtml, DispatchSnippet, S}
import xml.{Elem, NodeSeq}
import org.nfolkert.fssc.{Game, RecData, VisitData, UserData, Rectangle, MapGrid, Cluster, DataPoint}
import net.liftweb.util.Props
import net.liftweb.common.{Loggable, Full}
import org.nfolkert.fssc.model.{RecommendationType, PlayLevel, ViewType, UserVenueHistory, User}
import org.nfolkert.lib.{Util, SHtmlExt, T}

case class Session(token: String, user: User, userAgent: UserAgent) // TODO: additional information for current state (map bounds, zoom level, etc.)

case class UserAgent(name: String)
case object UserAgent {
  val iPhone = UserAgent("iPhone")
  val android = UserAgent("Android")
  val other = UserAgent("Other")
  def getUserAgent = S.request.flatMap(_.request.userAgent.map(identity)) match {
    case Full(u) if u.contains("iPhone") => UserAgent.iPhone
    case Full(u) if u.contains("Android") => UserAgent.android
    case _ => UserAgent.other
  }
}

object Session extends Loggable {

  object session extends SessionVar[Option[Session]](None)

  def setup(key: String) {
    val token = if (key == "token") Props.get("access.token.user").getOrElse("test") else key
    (if (token == "test") Some(User.createRecord.id("-1")) else UserData.getUser(token)).map(user => {
      logger.info("Logging in: " + user.id)
      Session.session(Some(Session(token, user, UserAgent.getUserAgent)))
    }).getOrElse({
      logger.info("Could not log in; unknown user")
      Session.session.remove
    })
  }

  def clear() {
    logger.info(Session.session.is.map(s=>"Logging out: " + s.user.id).getOrElse("Not logged in"))
    Session.session.remove()
  }

  def getOrRedirect: Session = Session.session.is.map(identity).orElse({
    logger.info("Not logged in; redirecting to index")
    S.redirectTo("/")
  }).get
}

class StrategicFoursquare extends DispatchSnippet with Loggable {
  def dispatch: DispatchIt = {
    case "welcome" => welcome _
    case "renderMap" => InSession().renderMap _
    case "renderWebMap" => InSession().renderWebMap _
    case "renderTouchMap" => InSession().renderTouchMap _
    case "preferences" => InSession().preferences _
    case "discovery" => InSession().discoveryRedirect _
    case _ => welcome _
  }

  def welcome(xhtml: NodeSeq): NodeSeq = T("Render Welcome") {
    val url = S.uri

    S.request.map(r=>{
      val p = Util.printReq
      logger.info(p._1)
      logger.info(p._2)
    })

    /*
    // Uncomment to dump the request information
    if (Session.session.is.isDefined) {
      logger.info("Already logged in; redirecting to game map")
      S.redirectTo("/discover")
    }
    */

    val oauth = UserData.oauth
    S.param("code").flatMap(code=>{
      tryo(oauth.accessTokenCaller(code).get)
    }).map(t=>{Session.setup(t); InSession().discoveryRedirect(xhtml)}).getOrElse({
      S.param("test").map(p=>{
        Session.setup(p); InSession().discoveryRedirect(xhtml)
      }).getOrElse({
        def renderLink(xhtml: NodeSeq): NodeSeq = {
          <a href={oauth.authorizeUrl}>{xhtml}</a>
        }
        bind("auth", xhtml,
             "link" -%> renderLink _)
      })
    })
  }

  case class InSession() {
    val Session(token, user, userAgent) = Session.getOrRedirect

    def discoveryRedirect(xhtml: NodeSeq): NodeSeq = {
      S.param("display") match {
        case Full("web") => S.redirectTo("/web_discover")
        case Full("touch") => S.redirectTo("/touch_discover")
        case _ => {
          user.getViewType match {
            case ViewType.web => S.redirectTo("/web_discover")
            case ViewType.touch => S.redirectTo("/touch_discover")
            case _ => {
              userAgent match {
                case UserAgent.iPhone => S.redirectTo("/touch_discover")
                case UserAgent.android => S.redirectTo("/touch_discover")
                case _ => S.redirectTo("/web_discover")
              }
            }
          }
        }
      }
      NodeSeq.Empty
    }

    def recommendationTypeWidget(callbackFn: RecommendationType => JsCmd, overrideVal: Option[RecommendationType]=None) = {
      val recType = overrideVal getOrElse user.getRecommendations
      val recommendationOpts = RecommendationType.values.map(v=>(v.name, v.desc))
      SHtml.ajaxSelect(recommendationOpts, Full(recType.name), (newVal) => callbackFn(RecommendationType.byName(newVal)))
    }

    def viewTypeWidget(callbackFn: ViewType => JsCmd, overrideVal: Option[ViewType]=None) = {
      val viewType = overrideVal getOrElse user.getViewType
      val viewOpts = ViewType.values.map(v=>(v.name, v.desc))
      SHtml.ajaxSelect(viewOpts, Full(viewType.name), (newType) => callbackFn(ViewType.byName(newType)))
    }

    def playLevelWidget(callbackFn: PlayLevel => JsCmd, overrideVal: Option[PlayLevel]=None) = {
      val level = overrideVal getOrElse user.getPlayLevel
      val levelOpts = PlayLevel.values.map(v=>(v.name, v.desc))
      SHtml.ajaxSelect(levelOpts, Full(level.name), (newLevel) => callbackFn(PlayLevel.byName(newLevel)))
    }

    def updateSearchCall(lat: Double, lng: Double) = "g4c.updateSearchPosition("+lat+","+lng+")"
    def setOpacityCall(opacity: Double) = "g4c.setOverlayOpacity("+opacity+")"
    def renderRecsCall(json: String) = "g4c.renderRecommendations([" + json + "])"
    def resetViewCall(lat: Double, lng: Double, zoom: Int) = "g4c.resetCenterAndZoom("+lat+","+lng+","+zoom+")"
    def renderMapCall(json: String) = "g4c.renderMap([" + json + "])"
    def redrawOverlaysCall = "g4c.redrawOverlays()"

    def preferences(xhtml: NodeSeq): NodeSeq = {
      val mapDetail = user.opacity.value
      bind("prefs", xhtml,
           "level" -%> playLevelWidget(newLevel => { user.setPlayLevel(newLevel).save; JsCmds.Noop }),
           "viewtypes" -%> viewTypeWidget(newType => { user.setViewType(newType).save; JsCmds.Noop }),
           "recommendations" -%> recommendationTypeWidget(newVal => { user.setRecommendations(newVal).save; JsCmds.Noop }),
           "opacity" -%> SHtmlExt.ajaxRange(0.333, 1.0, 0.05, mapDetail, (newVal) => {
             user.opacity(newVal).save; JsCmds.Noop
           }),
           "logout" -%> SHtml.ajaxButton("Logout", () => {Session.clear; JsCmds.RedirectTo("/")}),
           "refreshdata" -%> SHtml.ajaxButton("Refresh", () => {
             Session.setup(token)
             UserData.fetchUserVenueHistory(user.id.value, token)
             JsCmds.Alert("Data refreshed")
           }),
           "deletedata" -%> SHtml.ajaxButton("Clear", () => {
             UserVenueHistory.find(user.id.value).map(_.delete_!)
             Session.clear;
             JsCmds.RedirectTo("/")
           })
      )
    }

    def renderMap(xhtml: NodeSeq): NodeSeq = T("Render Map") {
      val visitPoints = MapGrid.sortPointsByVisits(UserData.getVisitedPoints(token, user))
      val clusters = T("Build Clusters") { Cluster.buildClusters2(visitPoints).toList.sortBy(-_.pts.size) }

      if (!clusters.isEmpty) {
        var clusterIdx = 0//clusters.length-1
        var gridSize = 250
        var opacity = 1.0
        var searchLatLng: Option[(Double, Double)] = None
        var showOverlayBorders = false
        var recType = "none"

        def generateCall(resetZoom: Boolean, redrawOverlays: Boolean) = T("Generate Map JS") {
          val cluster = if (clusterIdx < 0) {
            Cluster(visitPoints(0), visitPoints.toSet)
          } else
            clusters(clusterIdx)
          val pts = cluster.pts
          val bounds = cluster.bounds
          if (searchLatLng.isEmpty || resetZoom) {
            searchLatLng = Some((cluster.anchor.lat, cluster.anchor.lng))
          }

          val grid = MapGrid(gridSize, 1.5, pts)
          val sw = grid.snapPoint(bounds._1)
          val ne = grid.snapPoint(bounds._2)
          val boundRect = Rectangle(sw.lng, sw.lat, ne.lng+grid.lngSnap, ne.lat+grid.latSnap)

          val covered = T("Covered Cells") { grid.covered }
          val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }
          val recPts = searchLatLng.map(p => {
            val (lat, lng) = p
            UserData.getRecommendedPoints(lat, lng, grid, rects.toSet, recType, true, token).toList
          }).getOrElse(Nil)

          val center = (boundRect.bottom + .5 * boundRect.height, boundRect.left + .5 * boundRect.length)
          val breadth = math.max(boundRect.height, boundRect.length) * grid.metersInDegLat

          val eqScale = math.log(breadth).toInt
          val zoom = math.max(1, 18 - (eqScale-2))

          val debug = <div>
            <div>Current Position: {searchLatLng.map(_.toString).getOrElse("Unknown")}</div>
            <div>Total Point Count: {visitPoints.size}</div>
            <div>Cluster Point Count: {pts.size}</div>
            <div>Overlay Count: {rects.size}</div>
          </div>

          val score = Game.calculateScore(covered, grid.latSnap, grid.lngSnap)

          def overlaysJson = T("Overlays Json") { rects.map(_.toJson).join(",") }
          def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>RecData.pointToJson(pt)).join(",") }

          val call = "g4c.renderMap(\n" +
            (if (redrawOverlays) "[" + overlaysJson + "],\n" else "[],") +
            "[" + recommendationsJson + "],\n" +
            searchLatLng.map(p=>"[" + p._1 + "," + p._2 + "],").getOrElse("") +
            (if (resetZoom) {"[" + center._1 + "," + center._2 + "],"} else {"null,"}) +
            zoom + ", " +
            opacity + "," + redrawOverlays + ")"

          JsCmds.SetHtml("visited", <span>{score.visited}</span>) &
          JsCmds.SetHtml("totalPoints", <span>{score.total}</span>) &
          JsCmds.SetHtml("debug", debug) &
          JsCmds.Run(setOpacityCall(opacity)) &
          (if (redrawOverlays) JsCmds.Run(renderMapCall(overlaysJson)) else JsCmds.Noop) &
          JsCmds.Run(renderRecsCall(recommendationsJson)) &
          (if (resetZoom) JsCmds.Run(resetViewCall(center._1, center._2, zoom)) else JsCmds.Noop) &
          searchLatLng.map(p=>JsCmds.Run(updateSearchCall(p._1, p._2))).getOrElse(JsCmds.Noop)
        }

        val gridSizeOpts = List(10, 100, 250, 400, 800, 1000, 5000, 10000, 40000, 100000).map(m=>(m.toString, if (m < 1000) {m + " m"} else {m/1000 + " km"}))

        val recommendationOpts = List(
          ("none", "No Recommendations"),
          ("food", "Food"), ("drinks", "Drinks"), ("coffee", "Coffee"), ("shops", "Shopping"), ("arts", "Arts and Entertainment"), ("outdoors", "Outdoors"),
          ("all", "All Categories"))

        val clusterOpts = (1 to clusters.size).toList.zip(clusters).map(p=>((p._1-1).toString, VisitData.clusterName(p._2))) ++ List(((-1).toString, "ALL"))

        bind("map", xhtml,
             "cluster" -%> SHtml.ajaxSelect(clusterOpts, Full(clusterIdx.toString), (newCluster) => {
               clusterIdx = tryo(newCluster.toInt).openOr(0)
               generateCall(true, true)
             }),
             "gridsize" -%> SHtml.ajaxSelect(gridSizeOpts, Full("250"), (newVal) => {
               gridSize = tryo(newVal.toInt).openOr(gridSize)
               generateCall(false, true)
             }),
             "opacity" -%> SHtmlExt.ajaxRange(0.0, 1.0, 0.05, opacity, (newVal) => {
               opacity = newVal
               JsCmds.Run("g4c.updateOpacity(" + opacity + ")")
             }),
             "overlayborders" -%> SHtml.ajaxCheckbox(showOverlayBorders, (newVal) => {
               showOverlayBorders = newVal
               JsCmds.Run("g4c.showBorders(" + showOverlayBorders + ")")
             }),
             "searchlatlng" -%> SHtml.ajaxText("", (newVal) => {
               val asList = newVal.split(',').toList.flatMap(s=>tryo(s.toDouble))
               if (asList.size == 2) {
                 searchLatLng = Some(asList(0), asList(1))
                 generateCall(false, false)
               } else JsCmds.Noop
             }),
             "recommendations" -%> SHtml.ajaxSelect(recommendationOpts, Full("none"), (newVal) => {
               recType = newVal
               generateCall(false, false)
             })
        )
      }
      else
        xhtml
    }

  def renderWebMap(xhtml: NodeSeq): NodeSeq = {
    val visitPoints = MapGrid.sortPointsByVisits(UserData.getVisitedPoints(token, user))
    val clusters = T("Build Clusters") { Cluster.buildClusters2(visitPoints).toList.sortBy(-_.pts.size) }

    if (!clusters.isEmpty) {
      var clusterIdx = 0
      val gridSize = user.getPlayLevel.gridSize
      val opacity = user.opacity.value
      var searchLatLng: Option[(Double, Double)] = None
      val showOverlayBorders = false
      val recType = user.getRecommendations.name

      def generateCall(resetZoom: Boolean, redrawOverlays: Boolean) = T("Generate Map JS") {
        val cluster = if (clusterIdx < 0) {
          Cluster(visitPoints(0), visitPoints.toSet)
        } else
          clusters(clusterIdx)
        val pts = cluster.pts
        val bounds = cluster.bounds
        if (searchLatLng.isEmpty || resetZoom) {
          searchLatLng = Some((cluster.anchor.lat, cluster.anchor.lng))
        }

        val grid = MapGrid(gridSize, 1.5, pts)

        val covered = T("Covered Cells") { grid.covered }
        val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }
        val recPts = searchLatLng.map(p => {
          val (lat, lng) = p
          UserData.getRecommendedPoints(lat, lng, grid, rects.toSet, recType, true, token).toList
        }).getOrElse(Nil)

        val center = searchLatLng.getOrElse((cluster.anchor.lat, cluster.anchor.lng))

        val zoom = user.getPlayLevel.initialZoom

        val debug = <div>
          <div>Current Position: {searchLatLng.map(_.toString).getOrElse("Unknown")}</div>
          <div>Total Point Count: {visitPoints.size}</div>
          <div>Cluster Point Count: {pts.size}</div>
          <div>Overlay Count: {rects.size}</div>
        </div>

        val score = Game.calculateScore(covered, grid.latSnap, grid.lngSnap)

        def overlaysJson = T("Overlays Json") { rects.map(_.toJson).join(",") }
        def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>RecData.pointToJson(pt)).join(",") }

        JsCmds.SetHtml("visited", <span>{score.visited}</span>) &
        JsCmds.SetHtml("totalPoints", <span>{score.total}</span>) &
        JsCmds.SetHtml("debug", debug) &
        JsCmds.Run(setOpacityCall(opacity)) &
        (if (redrawOverlays) JsCmds.Run(renderMapCall(overlaysJson)) else JsCmds.Noop) &
        JsCmds.Run(renderRecsCall(recommendationsJson)) &
        (if (resetZoom) JsCmds.Run(resetViewCall(center._1, center._2, zoom)) else JsCmds.Noop) &
        searchLatLng.map(p=>JsCmds.Run(updateSearchCall(p._1, p._2))).getOrElse(JsCmds.Noop)
      }

      val clusterOpts = (1 to clusters.size).toList.zip(clusters).map(p=>((p._1-1).toString, VisitData.clusterName(p._2))) ++ List(((-1).toString, "ALL"))

      bind("map", xhtml,
           "cluster" -%> SHtml.ajaxSelect(clusterOpts, Full(clusterIdx.toString), (newCluster) => {
             clusterIdx = tryo(newCluster.toInt).openOr(0)
             generateCall(true, true)
           }),
           "searchlatlng" -%> SHtml.ajaxText("", (newVal) => {
             val asList = newVal.split(',').toList.flatMap(s=>tryo(s.toDouble))
             if (asList.size == 2) {
               searchLatLng = Some(asList(0), asList(1))
               generateCall(false, false)
             } else JsCmds.Noop

           })
      )
    }
    else
      xhtml
  }

  def renderTouchMap(xhtml: NodeSeq): NodeSeq = {
    val url = S.uri

    var initialLatLng: Option[(Double, Double)] = None
    var prevLatLng: Option[(Double, Double)] = None
    var currLatLng: Option[(Double, Double)] = None
    var searchLatLng: Option[(Double, Double)] = None
    var cluster: Option[Cluster[VisitData]] = None

    val gridSize = user.getPlayLevel.gridSize
    val opacity = user.opacity.value
    val showOverlayBorders = false
    val recType = user.getRecommendations.name

    def initializeMap {
      currLatLng.map {ll =>
        initialLatLng = Some(ll)
        prevLatLng = Some(ll)
        val allPoints = UserData.getVisitedPoints(token, user)
        val closePoints = allPoints.filter(_.distanceTo(ll._1, ll._2) < 3000)
        cluster = closePoints.headOption.map(a => Cluster(a, closePoints))
      }
    }

    def generateCall(resetZoom: Boolean, redrawOverlays: Boolean) = T("Generate Map JS") {
      (for {
        ll <- currLatLng
      } yield {
        val (pts, bounds) = (for {c <- cluster} yield {
          (c.pts, c.bounds)
        }).getOrElse((Set[DataPoint[VisitData]](), (DataPoint[VisitData](ll._1, ll._2), DataPoint[VisitData](ll._1, ll._2))))

        val grid = MapGrid(gridSize, 1.5, pts)

        val covered = T("Covered Cells") { grid.covered }
        val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }

        if (searchLatLng.isEmpty) {
          searchLatLng = Some(ll._1, ll._2)
        }

        val recPts = searchLatLng.map(ll => {
          val (lat, lng) = ll
          UserData.getRecommendedPoints(lat, lng, grid, rects.toSet, recType, false, token).toList
        }).getOrElse(Nil)

        val zoom = user.getPlayLevel.initialZoom

        val score = Game.calculateScore(covered, grid.latSnap, grid.lngSnap)

        def overlaysJson = T("Overlays Json") { rects.map(_.toJson).join(",") }
        def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>RecData.pointToJson(pt)).join(",") }

        JsCmds.Run(setOpacityCall(opacity)) &
        (if (redrawOverlays) JsCmds.Run(renderMapCall(overlaysJson)) else JsCmds.Noop) &
        JsCmds.Run(renderRecsCall(recommendationsJson)) &
        (if (resetZoom) JsCmds.Run(resetViewCall(ll._1, ll._2, zoom)) else JsCmds.Noop)
      }).getOrElse(JsCmds.Noop)
    }

    bind("map", xhtml,
         "currentlatlng" -%> SHtml.ajaxText("", (newVal)=>{
           val asList = newVal.split(',').toList.flatMap(s=>tryo(s.toDouble))
           if (asList.size == 2) {
             currLatLng = Some(asList(0), asList(1))
             if (prevLatLng.isEmpty ||
                 (for {ll1 <- initialLatLng; ll2 <- currLatLng} yield {DataPoint.distanceBetween(ll1._1, ll1._2, ll2._1, ll2._2) > 500}).getOrElse(true)) {
               initializeMap
               generateCall(true, true)
             } else {
               JsCmds.Noop
             }
           } else
             JsCmds.Noop
         }),
         "searchlatlng" -%> SHtml.ajaxText("", (newVal)=>{
           val asList = newVal.split(',').toList.flatMap(s=>tryo(s.toDouble))
           if (asList.size == 2) {
             searchLatLng = Some(asList(0), asList(1))
             generateCall(false, false)
           } else JsCmds.Noop
         })
    )
  }

  }
}
