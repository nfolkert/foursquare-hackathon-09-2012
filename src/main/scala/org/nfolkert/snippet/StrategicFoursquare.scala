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
import org.nfolkert.lib.{SHtmlExt, T}
import org.nfolkert.fssc.model.{RecommendationType, PlayLevel, ViewType, UserVenueHistory, User}

case class Session(token: String, user: User) // TODO: additional information for current state (map bounds, zoom level, etc.)

object Session extends Loggable {

  object session extends SessionVar[Option[Session]](None)

  def setup(key: String) {
    val token = if (key == "token") Props.get("access.token.user").getOrElse("test") else key
    (if (token == "test") Some(User.createRecord.id("-1")) else UserData.getUser(token)).map(user => {
      Session.session(Some(Session(token, user)))
    }).getOrElse({
      Session.session.remove
      logger.info("Could not set token; unknown user")
    })
  }

  def clear() {
    Session.session.remove()
  }

  def getOrRedirect: Session = Session.session.is.map(identity).orElse(S.redirectTo("/")).get
}

case class RecVenue(name: String, lat: Double, lng: Double, id: String, catIcon: Option[String], catName: Option[String], address: Option[String])

class StrategicFoursquare extends DispatchSnippet {
  implicit val formats = DefaultFormats

  def dispatch: DispatchIt = {
    case "renderMap" => renderMap _
    case "renderWebMap" => renderWebMap _
    case "renderTouchMap" => renderTouchMap _
    case "welcome" => welcome _
    case "preferences" => preferences _
    case "discovery" => discoveryRedirect _
    case _ => renderMap _
  }

  def welcome(xhtml: NodeSeq): NodeSeq = T("Render Welcome") {
    val url = S.uri

    if (Session.session.is.isDefined) S.redirectTo("/discover")

    val oauth = UserData.oauth
    S.param("code").flatMap(code=>{
      tryo(oauth.accessTokenCaller(code).get)
    }).map(t=>{Session.setup(t); discoveryRedirect(xhtml)}).getOrElse({
      S.param("test").map(p=>{
        Session.setup(p); discoveryRedirect(xhtml)
      }).getOrElse({
        def renderLink(xhtml: NodeSeq): NodeSeq = {
          <a href={oauth.authorizeUrl}>{xhtml}</a>
        }

        bind("auth", xhtml,
             "link" -%> renderLink _)
      })
    })
  }

  def discoveryRedirect(xhtml: NodeSeq): NodeSeq = {
    S.param("display") match {
      case Full("web") => S.redirectTo("/web_discover")
      case Full("touch") => S.redirectTo("/touch_discover")
      case _ => {
        Session.session.is.map(_.user.getViewType) match {
          case Some(ViewType.web) => S.redirectTo("/web_discover")
          case Some(ViewType.touch) => S.redirectTo("/touch_discover")
          case _ => {
            S.request.flatMap(_.userAgent) match {
              case Full(v) if v.indexOf("iPhone") >= 0 => S.redirectTo("/touch_discover")
              case Full(v) if v.indexOf("Android") >= 0 => S.redirectTo("/touch_discover")
              case _ => S.redirectTo("/web_discover")
            }
          }
        }
      }
    }
    NodeSeq.Empty
  }

  def preferences(xhtml: NodeSeq): NodeSeq = {
    val Session(token, user) = Session.getOrRedirect

    val viewType = user.getViewType
    val level = user.getPlayLevel
    val recs = user.getRecommendations
    val mapDetail = user.opacity.value

    val levelOpts = PlayLevel.values.map(v=>(v.name, v.desc))
    val viewOpts = ViewType.values.map(v=>(v.name, v.desc))
    val recommendationOpts = RecommendationType.values.map(v=>(v.name, v.desc))

    bind("prefs", xhtml,
         "level" -%> SHtml.ajaxSelect(levelOpts, Full(level.name), (newLevel) => {
           PlayLevel.values.find(_.name == newLevel).map(nl=>user.setPlayLevel(nl).save); JsCmds.Noop
         }),
         "viewtypes" -%> SHtml.ajaxSelect(viewOpts, Full(viewType.name), (newType) => {
           ViewType.values.find(_.name == newType).map(nt=>user.setViewType(nt).save); JsCmds.Noop
         }),
         "opacity" -%> SHtmlExt.ajaxRange(0.333, 1.0, 0.05, mapDetail, (newVal) => {
           user.opacity(newVal).save; JsCmds.Noop
         }),
         "recommendations" -%> SHtml.ajaxSelect(recommendationOpts, Full(recs.name), (newVal) => {
           RecommendationType.values.find(_.name == newVal).map(rc=>user.setRecommendations(rc).save); JsCmds.Noop
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
    val Session(token, user) = Session.getOrRedirect

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
          UserData.getRecommendedPoints(lat, lng, rects.toSet, recType, token).toList
        }).getOrElse(Nil)

        def recPointToJson(pt: DataPoint[RecData]): Option[String] = {
          pt.data.flatMap(d => {
            val cat = d.venue.categories.find(_.primary.getOrElse(false))
            val catIcon = cat.map(_.icon)
            val catName = cat.map(_.name)
            val name = d.venue.name
            val id = d.venue.id
            val address = d.venue.location.address
            for {lat <- d.venue.location.lat; lng <- d.venue.location.lng} yield {
              val json = Extraction.decompose(RecVenue(name, lat, lng, id, catIcon, catName, address))
              Printer.compact(JsonAST.render(json))
            }
          })
        }

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
        def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>recPointToJson(pt)).join(",") }

        val call = "renderMap(\n" +
          (if (redrawOverlays) "[" + overlaysJson + "],\n" else "[],") +
          "[" + recommendationsJson + "],\n" +
          searchLatLng.map(p=>"[" + p._1 + "," + p._2 + "],").getOrElse("") +
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
             JsCmds.Run("updateOpacity(" + opacity + ")")
           }),
           "overlayborders" -%> SHtml.ajaxCheckbox(showOverlayBorders, (newVal) => {
             showOverlayBorders = newVal
             JsCmds.Run("showBorders(" + showOverlayBorders + ")")
           }),
           "logout" -%> SHtml.ajaxButton("Logout", () => {Session.clear; JsCmds.RedirectTo("/")}),
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
           }),
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
    else
      xhtml
  }

  def renderWebMap(xhtml: NodeSeq): NodeSeq = {
    val Session(token, user) = Session.getOrRedirect

    val visitPoints = MapGrid.sortPointsByVisits(UserData.getVisitedPoints(token, user))
    val clusters = T("Build Clusters") { Cluster.buildClusters2(visitPoints).toList.sortBy(-_.pts.size) }

    def clusterName(cluster: Cluster[VisitData]): String = {
      val grouped = cluster.pts.toList.flatMap(_.data).map(_.name).groupBy(n=>n).toList.sortBy(-_._2.size)
      grouped.map(_._1).headOption.getOrElse(cluster.anchor.lat + ", " + cluster.anchor.lng)
    }

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
        val sw = grid.snapPoint(bounds._1)
        val ne = grid.snapPoint(bounds._2)
        val boundRect = Rectangle(sw.lng, sw.lat, ne.lng+grid.lngSnap, ne.lat+grid.latSnap)

        val covered = T("Covered Cells") { grid.covered }
        val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }
        val recPts = searchLatLng.map(p => {
          val (lat, lng) = p
          UserData.getRecommendedPoints(lat, lng, rects.toSet, recType, token).toList
        }).getOrElse(Nil)

        def recPointToJson(pt: DataPoint[RecData]): Option[String] = {
          pt.data.flatMap(d => {
            val cat = d.venue.categories.find(_.primary.getOrElse(false))
            val catIcon = cat.map(_.icon)
            val catName = cat.map(_.name)
            val name = d.venue.name
            val id = d.venue.id
            val address = d.venue.location.address
            for {lat <- d.venue.location.lat; lng <- d.venue.location.lng} yield {
              val json = Extraction.decompose(RecVenue(name, lat, lng, id, catIcon, catName, address))
              Printer.compact(JsonAST.render(json))
            }
          })
        }

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
        def recommendationsJson = T("Recommendations Json") { recPts.flatMap(pt=>recPointToJson(pt)).join(",") }

        val call = "renderMap(\n" +
          (if (redrawOverlays) "[" + overlaysJson + "],\n" else "[],") +
          "[" + recommendationsJson + "],\n" +
          searchLatLng.map(p=>"[" + p._1 + "," + p._2 + "],").getOrElse("") +
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
    val Session(token, user) = Session.getOrRedirect
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
        val sw = grid.snapPoint(bounds._1)
        val ne = grid.snapPoint(bounds._2)
        val boundRect = Rectangle(sw.lng, sw.lat, ne.lng+grid.lngSnap, ne.lat+grid.latSnap)

        val covered = T("Covered Cells") { grid.covered }
        val rects = T("Grid Decomposition") { Rectangle.sortByLeft(grid.decompose.toList) }


        val recPts = searchLatLng.map(ll => {
          val (lat, lng) = ll
          UserData.getRecommendedPoints(lat, lng, rects.toSet, recType, token).toList
        }).getOrElse(Nil)

        def recPointToJson(pt: DataPoint[RecData]): Option[String] = {
          pt.data.flatMap(d => {
            val cat = d.venue.categories.find(_.primary.getOrElse(false))
            val catIcon = cat.map(_.icon)
            val catName = cat.map(_.name)
            val name = d.venue.name
            val id = d.venue.id
            val address = d.venue.location.address
            for {lat <- d.venue.location.lat; lng <- d.venue.location.lng} yield {
              val json = Extraction.decompose(RecVenue(name, lat, lng, id, catIcon, catName, address))
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
