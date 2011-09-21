package org.nfolkert.snippet

import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import net.liftweb.http.js.{JsCmds, JsCmd, JE}
import org.scalafoursquare.auth.OAuthFlow
import net.liftweb.http.{SessionVar, SHtml, DispatchSnippet, S, StatefulSnippet}
import xml.{Elem, Unparsed, NodeSeq, Text}
import net.liftweb.json.{DefaultFormats, JsonAST, Printer, Extraction}
import org.nfolkert.fssc.{Game, RecData, VisitData, UserData, Rectangle, MapGrid, Cluster, DataPoint}
import net.liftweb.util.Props
import net.liftweb.common.{Loggable, Full}
import org.nfolkert.lib.T
import org.nfolkert.fssc.model.{UserVenueHistory, User}

object Session extends Loggable {
  object userToken extends SessionVar[Option[String]](None)
  object user extends SessionVar[Option[User]](None)

  def setup(key: String) {
    val token = if (key == "token") Props.get("access.token.user").getOrElse("test") else key
    logger.info("Setting session token to " + token)
    Session.userToken(Some(token))
    if (token == "test")
      Session.user(Some(User.createRecord.id("-1")))
    else
      Session.user(UserData.getUser(token))
  }

  def clear() {
    Session.userToken.remove
    Session.user.remove
  }
}

case class RecVenue(name: String, lat: Double, lng: Double, catIcon: Option[String], catName: Option[String], address: Option[String])

class StrategicFoursquare extends DispatchSnippet {
  implicit val formats = DefaultFormats

  def dispatch: DispatchIt = {
    case "renderMap" => renderMap _
    case "renderTouchMap" => renderTouchMap _
    case "welcome" => welcome _
    case _ => renderMap _
  }

  def welcome(xhtml: NodeSeq): NodeSeq = T("Render Welcome") {
    val url = S.uri

    if (Session.userToken.is.isDefined) S.redirectTo("/map")

    val oauth = UserData.oauth
    S.param("code").flatMap(code=>{
      tryo(oauth.accessTokenCaller(code).get)
    }).map(t=>{Session.setup(t); S.redirectTo("/map")}).getOrElse({
      S.param("test").map(p=>{
        Session.setup(p); S.redirectTo("/map")
      }).getOrElse({
        def renderLink(xhtml: NodeSeq): NodeSeq = {
          <a href={oauth.authorizeUrl}>{xhtml}</a>
        }

        bind("auth", xhtml,
             "link" -%> renderLink _)
      })
    })
  }

  def renderMap(xhtml: NodeSeq): NodeSeq = T("Render Map") {
    val token = (Session.userToken.is.getOrElse(S.redirectTo("/")))
    val user = (Session.user.is.getOrElse(User.createRecord.id("-1")))
    
    val visitPoints = MapGrid.sortPointsByVisits(UserData.getVisitedPoints(token, user))
    val clusters = T("Build Clusters") { Cluster.buildClusters2(visitPoints).toList.sortBy(-_.pts.size) }

    def clusterName(cluster: Cluster[VisitData]): String = {
      val grouped = cluster.pts.toList.flatMap(_.data).map(_.name).groupBy(n=>n).toList.sortBy(-_._2.size)
      grouped.map(_._1).headOption.getOrElse(cluster.anchor.lat + ", " + cluster.anchor.lng)
    }

    if (!clusters.isEmpty) {
      var clusterIdx = 0//clusters.length-1
      var gridSize = 250
      var opacity = 1.0
      var currLatLng: Option[(Double, Double)] = None
      var showOverlayBorders = false
      var recType = "none"

      def generateCall(resetZoom: Boolean, redrawOverlays: Boolean) = T("Generate Map JS") {
        val cluster = if (clusterIdx < 0) {
          Cluster(visitPoints(0), visitPoints.toSet)
        } else
          clusters(clusterIdx)
        val pts = cluster.pts
        val bounds = cluster.bounds
        if (currLatLng.isEmpty || resetZoom) {
          currLatLng = Some((cluster.anchor.lat, cluster.anchor.lng))
        }

        val grid = MapGrid(gridSize, 1.5, pts)
        val sw = grid.snapPoint(bounds._1)
        val ne = grid.snapPoint(bounds._2)
        val boundRect = Rectangle(sw.lng, sw.lat, ne.lng+grid.lngSnap, ne.lat+grid.latSnap)

        val covered = T("Covered Cells") { grid.covered }
        val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }
        val recPts = currLatLng.map(p => {
          val (lat, lng) = p
          UserData.getRecommendedPoints(lat, lng, rects.toSet, recType, token).toList
        }).getOrElse(Nil)

        def recPointToJson(pt: DataPoint[RecData]): Option[String] = {
          pt.data.flatMap(d => {
            val cat = d.venue.categories.find(_.primary.getOrElse(false))
            val catIcon = cat.map(_.icon)
            val catName = cat.map(_.name)
            val name = d.venue.name
            val address = d.venue.location.address
            for {lat <- d.venue.location.lat; lng <- d.venue.location.lng} yield {
              val json = Extraction.decompose(RecVenue(name, lat, lng, catIcon, catName, address))
              Printer.compact(JsonAST.render(json))
            }
          })
        }

        val center = (boundRect.bottom + .5 * boundRect.height, boundRect.left + .5 * boundRect.length)
        val breadth = math.max(boundRect.height, boundRect.length) * grid.metersInDegLat

        val eqScale = math.log(breadth).toInt
        val zoom = math.max(1, 18 - (eqScale-2))

        val debug = <div>
          <div>Current Position: {currLatLng.map(_.toString).getOrElse("Unknown")}</div>
          <div>Total Point Count: {visitPoints.size}</div>
          <div>Cluster Point Count: {pts.size}</div>
          <div>Overlay Count: {rects.size}</div>
        </div>
        /*
          <div>Points: {visitPoints.toString}</div>
          <div>Cluster: {cluster.toString}</div>
          <div>Overlays: {rects.toString}</div>
        </div>
        */

        val score = Game.calculateScore(covered, grid.latSnap, grid.lngSnap)

        def overlaysJson = T("Overlays Json") { rects.map(_.toJson).join(",") }
        def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>recPointToJson(pt)).join(",") }

        val call = "renderMap(\n" +
          (if (redrawOverlays) "[" + overlaysJson + "],\n" else "[],") +
          "[" + recommendationsJson + "],\n" +
          currLatLng.map(p=>"[" + p._1 + "," + p._2 + "],").getOrElse("") +
          (if (resetZoom) {"[" + center._1 + "," + center._2 + "],"} else {"null,"}) +
          zoom + ", " +
          opacity + "," + redrawOverlays + ")"

        val showScore = "renderScore(" + score.visited + "," + score.total + ")"
        JsCmds.SetHtml("visited", <span>{score.visited}</span>) &
        JsCmds.SetHtml("totalPoints", <span>{score.total}</span>) &
        JsCmds.SetHtml("debug", debug) &
        JsCmds.Run(call)
      }

      val gridSizeOpts = List(10, 100, 250, 400, 800, 1000, 5000, 10000, 40000, 100000).map(m=>(m.toString, if (m < 1000) {m + " m"} else {m/1000 + " km"}))

      val recommendationOpts = List(
        ("none", "No Recommendations"),
        ("food", "Food"), ("drinks", "Drinks"), ("coffee", "Coffee"), ("shops", "Shopping"), ("arts", "Arts and Entertainment"), ("outdoors", "Outdoors"),
        ("all", "All Categories")
      )

      val clusterOpts = (1 to clusters.size).toList.zip(clusters).map(p=>((p._1-1).toString, clusterName(p._2))) ++ List(((-1).toString, "ALL"))

      def ajaxRange(min: Double, max: Double, step: Double, value: Double, fn: Double => JsCmd, attrs: SHtml.ElemAttr*): Elem = {
        // There is no lift ajax range slider; only a regular range slider.  Wah.
        import net.liftweb.util.Helpers._
        import net.liftweb.http.js.JE.JsRaw

        val fHolder = S.LFuncHolder(in => in.headOption.flatMap(asDouble(_)).map(fn(_)).getOrElse(JsCmds.Noop))

        val raw = (funcName: String, value: String) => JsRaw("'" + funcName + "=' + encodeURIComponent(" + value + ".value)")
        val key = S.formFuncName

        S.fmapFunc(S.contextFuncBuilder(fHolder)) {
          funcName =>
            <input type="range" min={min.toString} max={max.toString} step={step.toString} value={value.toString} onchange={SHtml.makeAjaxCall(raw(funcName, "this")).toJsCmd}/>
        }
      }

      bind("map", xhtml,
           "setup" -> {
             val call = SHtml.ajaxCall(JE.JsRaw(""), (ignored) => {
               generateCall(true, true)
             })._2.toJsCmd
             <script type="text/javascript">{call}</script>
           },
           "cluster" -%> SHtml.ajaxSelect(clusterOpts, Full(clusterIdx.toString), (newCluster) => {
             clusterIdx = tryo(newCluster.toInt).openOr(0)
             generateCall(true, true)
           }),
           "gridsize" -%> SHtml.ajaxSelect(gridSizeOpts, Full("250"), (newVal) => {
             gridSize = tryo(newVal.toInt).openOr(gridSize)
             generateCall(false, true)
           }),
           "opacity" -%> ajaxRange(0.0, 1.0, 0.05, opacity, (newVal) => {
             opacity = newVal
             JsCmds.Run("updateOpacity(" + opacity + ")")
           }),
           "overlayborders" -%> SHtml.ajaxCheckbox(showOverlayBorders, (newVal) => {
             showOverlayBorders = newVal
             JsCmds.Run("showBorders(" + showOverlayBorders + ")")
           }),
           "logout" -%> SHtml.ajaxButton("Logout", () => {Session.clear; JsCmds.RedirectTo("/")}),
           "currentlatlng" -%> SHtml.ajaxText("", (newVal)=>{
             val asList = newVal.split(',').toList.flatMap(s=>tryo(s.toDouble))
             if (asList.size == 2) {
               currLatLng = Some(asList(0), asList(1))
               generateCall(false, false)
             } else JsCmds.Noop
           }),
           "recommendations" -%> SHtml.ajaxSelect(recommendationOpts, Full("none"), (newVal) => {
             recType = newVal
             generateCall(false, false)
           }),
           "refreshdata" -%> SHtml.ajaxButton("Refresh", () => {
             Session.user.is.map(user=>{
               Session.setup(token)
               UserData.fetchUserVenueHistory(user.id.value, token)
               JsCmds.Alert("Data refreshed")
             }).getOrElse(JsCmds.Noop)
           }),
           "deletedata" -%> SHtml.ajaxButton("Clear", () => {
             Session.user.is.map(user=>{
               UserVenueHistory.find(user.id.value).map(_.delete_!)
               Session.clear;
               JsCmds.RedirectTo("/")
             }).getOrElse(JsCmds.Noop)
           })
      )
    }
    else
      xhtml
  }

  def renderTouchMap(xhtml: NodeSeq): NodeSeq = {
    val token = (Session.userToken.is.getOrElse(S.redirectTo("/")))
    val user = (Session.user.is.getOrElse(User.createRecord.id("-1")))
    val url = S.uri

    var initialLatLng: Option[(Double, Double)] = None
    var prevLatLng: Option[(Double, Double)] = None
    var currLatLng: Option[(Double, Double)] = None
    var cluster: Option[Cluster[VisitData]] = None

    // Set these up in preferences
    val gridSize = 250
    val opacity = 1.0
    val showOverlayBorders = false
    val recType = "none"

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
        val sw = grid.snapPoint(bounds._1)
        val ne = grid.snapPoint(bounds._2)
        val boundRect = Rectangle(sw.lng, sw.lat, ne.lng+grid.lngSnap, ne.lat+grid.latSnap)

        val covered = T("Covered Cells") { grid.covered }
        val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }

        val (lat, lng) = ll
        val recPts = UserData.getRecommendedPoints(lat, lng, rects.toSet, recType, token).toList

        def recPointToJson(pt: DataPoint[RecData]): Option[String] = {
          pt.data.flatMap(d => {
            val cat = d.venue.categories.find(_.primary.getOrElse(false))
            val catIcon = cat.map(_.icon)
            val catName = cat.map(_.name)
            val name = d.venue.name
            val address = d.venue.location.address
            for {lat <- d.venue.location.lat; lng <- d.venue.location.lng} yield {
              val json = Extraction.decompose(RecVenue(name, lat, lng, catIcon, catName, address))
              Printer.compact(JsonAST.render(json))
            }
          })
        }

        val center = (boundRect.bottom + .5 * boundRect.height, boundRect.left + .5 * boundRect.length)
        val breadth = (math.max(boundRect.height, boundRect.length) * grid.metersInDegLat) / 10

        val eqScale = math.log(breadth).toInt
        val zoom = math.max(1, 18 - (eqScale-2))

        val score = Game.calculateScore(covered, grid.latSnap, grid.lngSnap)

        def overlaysJson = T("Overlays Json") { rects.map(_.toJson).join(",") }
        def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>recPointToJson(pt)).join(",") }

        val call = "renderMap(\n" +
          (if (redrawOverlays) "[" + overlaysJson + "],\n" else "[],") +
          "[" + recommendationsJson + "],\n" +
          "null," +
          (if (resetZoom) {"[" + center._1 + "," + center._2 + "],"} else {"null,"}) +
          zoom + ", " +
          opacity + "," + redrawOverlays + ")"

        JsCmds.Run(call)
      }).getOrElse(JsCmds.Noop)
    }

    bind("map", xhtml,
         "currentlatlng" -%> SHtml.ajaxText("", (newVal)=>{
           val asList = newVal.split(',').toList.flatMap(s=>tryo(s.toDouble))
           if (asList.size == 2) {
             currLatLng = Some(asList(0), asList(1))
             if (prevLatLng.isEmpty ||
                 (for {ll1 <- initialLatLng; ll2 <- currLatLng} yield {DataPoint.distanceBetween(ll1._1, ll1._2, ll2._1, ll2._2) > 1000}).getOrElse(true)) {
               initializeMap
               generateCall(true, true)
             }
             else if ((for {ll1 <- prevLatLng; ll2 <- currLatLng} yield {DataPoint.distanceBetween(ll1._1, ll1._2, ll2._1, ll2._2) > 50}).getOrElse(false)) {
               generateCall(false, false)
             } else {
               JsCmds.Noop
             }
           } else
             JsCmds.Noop
         })
    )
  }
}
