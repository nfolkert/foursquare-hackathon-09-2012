package org.nfolkert.snippet

import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import net.liftweb.common.Full
import net.liftweb.http.js.{JsCmds, JsCmd, JE}
import org.scalafoursquare.auth.OAuthFlow
import net.liftweb.http.{SessionVar, SHtml, DispatchSnippet, S, StatefulSnippet}
import xml.{Elem, Unparsed, NodeSeq, Text}
import org.nfolkert.fssc.{VisitData, VisitedPoints, Rectangle, MapGrid, Cluster, DataPoint}

object Session {
  object userToken extends SessionVar[Option[String]](None)
}

class StrategicFoursquare extends DispatchSnippet {

  def dispatch: DispatchIt = {
    case "renderMap" => renderMap _
    case _ => renderMap _
  }

  def renderMap(xhtml: NodeSeq): NodeSeq = {
    val url = S.uri
    
    // Finding user token
    val token = Session.userToken.is.getOrElse({
      val oauth = VisitedPoints.oauth
      // Are we on return from an oauth call?
      S.param("code").flatMap(code=>{
        tryo(oauth.accessTokenCaller(code).get)
      }).map(t=>{Session.userToken(Some(t)); S.redirectTo(url)}).getOrElse({
        // Are we in testing mode?
        // TODO - check params; use either test data, or lookup based on test token in props
        S.param("test").map(p=>{
          if (p == "token")
            VisitedPoints.AUTH_TOKEN
          else
            "test"
        }).openOr({
          // Otherwise call the authorize url
          val url = oauth.authorizeUrl
          S.redirectTo(url)
        })
      })
    })

    val visitPoints = VisitedPoints.getVisitedPoints(token)
    val clusters = Cluster.buildClusters(visitPoints).toList.sortBy(-_.pts.size)

    def clusterName(cluster: Cluster[VisitData]): String = {
      val grouped = cluster.pts.toList.flatMap(_.data).map(_.name).groupBy(n=>n).toList.sortBy(-_._2.size)
      grouped.map(_._1).headOption.getOrElse(cluster.anchor.lat + ", " + cluster.anchor.lng)
    }

    if (!clusters.isEmpty) {
      var clusterIdx = 0
      var gridSize = 333
      var opacity = 1.0
      var showOverlayBorders = false

      def generateCall(resetZoom: Boolean) = {
        val cluster = if (clusterIdx < 0) {
          Cluster(visitPoints.toList(0), visitPoints)
        } else
          clusters(clusterIdx)
        val pts = cluster.pts
        val bounds = cluster.bounds

        val grid = MapGrid(gridSize, 1.5, pts)
        val sw = grid.snapPoint(bounds._1)
        val ne = grid.snapPoint(bounds._2)
        val boundRect = Rectangle(sw.lng, sw.lat, ne.lng+grid.lngSnap, ne.lat+grid.latSnap)

        val rects = Rectangle.sortByLeft(grid.decompose.toList)

        val center = (boundRect.bottom + .5 * boundRect.height, boundRect.left + .5 * boundRect.length)
        val breadth = math.max(boundRect.height, boundRect.length) * grid.metersInDegLat

        val eqScale = math.log(breadth).toInt
        val zoom = math.max(1, 18 - (eqScale-2))

        val debug = <div>
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

        val call = "renderMap(\n" +
          "[" + rects.map(_.toJson).join(",") + "],\n" +
          (if (resetZoom) {"[" + center._1 + "," + center._2 + "],"} else {"null,"}) +
          zoom + ", " +
          opacity + ")"

        JsCmds.SetHtml("debug", debug) &
        JsCmds.Run(call)
      }

      val gridSizeOpts = List(10, 100, 250, 400, 800, 1000, 5000, 10000, 40000, 100000).map(m=>(m.toString, if (m < 1000) {m + " m"} else {m/1000 + " km"}))

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
               generateCall(true)
             })._2.toJsCmd
             <script type="text/javascript">{call}</script>
           },
           "cluster" -%> SHtml.ajaxSelect(clusterOpts, Full("0"), (newCluster) => {
             clusterIdx = tryo(newCluster.toInt).openOr(0)
             generateCall(true)
           }),
           "gridsize" -%> SHtml.ajaxSelect(gridSizeOpts, Full("250"), (newVal) => {
             gridSize = tryo(newVal.toInt).openOr(gridSize)
             generateCall(false)
           }),
           "opacity" -%> ajaxRange(0.0, 1.0, 0.05, opacity, (newVal) => {
             opacity = newVal
             JsCmds.Run("updateOpacity(" + opacity + ")")
           }),
           "overlayborders" -%> SHtml.ajaxCheckbox(showOverlayBorders, (newVal) => {
             showOverlayBorders = newVal
             JsCmds.Run("showBorders(" + showOverlayBorders + ")")
           }),
           "logout" -%> SHtml.ajaxButton("Logout", () => {Session.userToken.remove(); JsCmds.RedirectTo(url)})
      )
    }
    else
      xhtml
  }
}
