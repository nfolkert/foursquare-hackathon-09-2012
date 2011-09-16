package org.nfolkert.snippet

import net.liftweb.util.Helpers._
import xml.{Unparsed, NodeSeq, Text}
import org.joda.time.DateTime
import net.liftweb.http.{SHtml, DispatchSnippet, S, StatefulSnippet}
import net.liftweb.common.Full
import net.liftweb.http.js.{JsCmds, JsCmd, JE}
import org.nfolkert.fssc.{VisitedPoints, Rectangle, MapGrid, Cluster, VPt}
import org.scalafoursquare.auth.OAuthFlow

class StrategicFoursquare extends DispatchSnippet {

  def dispatch: DispatchIt = {
    case "renderMap" => renderMap _
    case _ => renderMap _
  }

  def renderMap(xhtml: NodeSeq): NodeSeq = {

    val visitPoints = VisitedPoints.getVisitedPoints
    val clusters = Cluster.buildClusters(visitPoints).toList.sortBy(-_.pts.size)

    if (!clusters.isEmpty) {
      var clusterIdx = 0
      var gridSize = 333
      var opacity = 1.0

      def generateCall() = {
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
         */

        val call = "renderMap(\n" +
          "[" + rects.map(_.toJson).join(",") + "],\n" +
          "[" + center._1 + "," + center._2 + "]," +
          zoom + ", " +
          opacity + ")"

        JsCmds.SetHtml("debug", debug) &
        JsCmds.Run(call)
      }

      val clusterOpts = (1 to clusters.size).toList.map(idx => ((idx-1).toString, idx.toString)) ++ List(((-1).toString, "ALL"))

      bind("map", xhtml,
           "setup" -> {
             val call = SHtml.ajaxCall(JE.JsRaw(""), (ignored) => {
               generateCall()
             })._2.toJsCmd
             <script type="text/javascript">{call}</script>
           },
           "cluster" -%> SHtml.ajaxSelect(clusterOpts, Full("0"), (newCluster) => {
             clusterIdx = tryo(newCluster.toInt).openOr(0)
             generateCall()
           }),
           "gridsize" -%> SHtml.ajaxText(gridSize.toString, (newVal) => {
             gridSize = tryo(newVal.toInt).openOr(gridSize)
             generateCall()
           }),
           "opacity" -%> SHtml.ajaxText(opacity.toString, (newVal) => {
             opacity = tryo(newVal.toDouble).openOr(opacity)
             generateCall()
           })
      )
    }
    else
      xhtml
  }
}
