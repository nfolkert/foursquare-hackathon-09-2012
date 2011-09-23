package org.nfolkert.fssc

import org.joda.time.DateTime
import org.scalafoursquare.response.VenueCompact
import net.liftweb.json.{DefaultFormats, JsonAST, Printer, Extraction}

case class DataPoint[T](lat: Double, lng: Double, data: Option[T]=None) {

  def distanceTo(pt: DataPoint[_]): Double = {
    distanceTo(pt.lat, pt.lng)
  }

  def distanceTo(l: Double, g: Double): Double = {
    DataPoint.distanceBetween(lat, lng, l, g)
  }
}

object DataPoint {
  def distanceBetween(l1: Double, g1: Double, l2: Double, g2: Double): Double = {
    val theta = g1 - g2
    val distance = math.sin(math.toRadians(l1)) * math.sin(math.toRadians(l2)) +
                   math.cos(math.toRadians(l1)) * math.cos(math.toRadians(l2)) * math.cos(math.toRadians(theta))
    math.floor(6378100 * math.acos(distance))
  }
}

case class VisitData(visits: Int=0, name: String="", lastVisit: DateTime=new DateTime) {}

object VisitData {
  def clusterName(cluster: Cluster[VisitData]) = {
    val grouped = cluster.pts.toList.flatMap(_.data).map(_.name).groupBy(n=>n).toList.sortBy(-_._2.size)
    grouped.map(_._1).headOption.getOrElse(cluster.anchor.lat + ", " + cluster.anchor.lng)
  }
}

case class RecData(venue: VenueCompact) {
  def toJson = RecData.toJson(this)
}
case class RecVenue(name: String, lat: Double, lng: Double, id: String, catIcon: Option[String], catName: Option[String], address: Option[String])

object RecData {
  implicit val formats = DefaultFormats

  def toJson(data: RecData) = {
    val cat = data.venue.categories.find(_.primary.getOrElse(false))
    val catIcon = cat.map(_.icon)
    val catName = cat.map(_.name)
    val name = data.venue.name
    val id = data.venue.id
    val address = data.venue.location.address
    for {lat <- data.venue.location.lat; lng <- data.venue.location.lng} yield {
      val json = Extraction.decompose(RecVenue(name, lat, lng, id, catIcon, catName, address))
      Printer.compact(JsonAST.render(json))
    }
  }

  def pointToJson(pt: DataPoint[RecData]) = {
    pt.data.flatMap(_.toJson)
  }
}

case class Rectangle(left: Double, bottom: Double, right: Double, top: Double) {
  def isEmpty = right <= left || top <= bottom
  def length = right - left
  def height = top - bottom
  def area = length * height

  def contains(point: DataPoint[_]): Boolean = {
    left <= point.lng && point.lng <= right && bottom <= point.lat && point.lat <= top
  }

  def intersect(rect: Rectangle): Rectangle = {
    val minTop = math.min(rect.top, top)
    val minRight = math.min(rect.right, right)
    val maxLeft = math.max(rect.left, left)
    val maxBottom = math.max(rect.bottom, bottom)

    if (minRight <= maxLeft || minTop <= maxBottom) return new Rectangle(0, 0, 0, 0)
    else return new Rectangle(maxLeft, maxBottom, minRight, minTop)
  }

  def intersects(rect: Rectangle): Boolean = {
    math.min(rect.top, top) > math.max(rect.bottom, bottom) &&
    math.min(rect.right, right) > math.max(rect.left, left)
  }

  def difference(rect: Rectangle, keepCenter: Boolean): List[Rectangle] = {
    if (!intersects(rect))
      List(this)
    else {
      val ins = intersect(rect)
      def addR(dr: Rectangle) = {
        if (dr.isEmpty)
          None
        else
          Some(dr)
      }

      // Difference does vertical striping always, so merge is easier
      val ret = List(
        addR(Rectangle(left, bottom, ins.left, top)),
        addR(Rectangle(ins.left, ins.top, ins.right, top)),
        addR(Rectangle(ins.right, bottom, right, top)),
        addR(Rectangle(ins.left, bottom, ins.right, ins.bottom))
      ).flatten
      if (keepCenter)
        ins :: ret
      else
        ret
    }
  }

  def toJson = "{l:" + left + ",b:" + bottom + ",r:" + right + ",t:" + top + "}"
}

object Rectangle {
  def decompose(srcRects: List[Rectangle], diffRects: List[Rectangle], keepCenters: Boolean): List[Rectangle] = {
    diffRects.foldLeft(srcRects)((decomp, diffRect) => {
      decomp.flatMap(srcRect => {
        srcRect.difference(diffRect, keepCenters)
      })
    })
  }

  def sortByLeft(rects: List[Rectangle]): List[Rectangle] = {
    rects.sortBy(r=>(r.left, r.top))
  }

  def sortByTop(rects: List[Rectangle]): List[Rectangle] = {
    rects.sortBy(r=>(r.top, r.left))
  }

  def merge(rects: List[Rectangle]): List[Rectangle] = {
    val firstRound = Rectangle.sortByLeft(rects)

    val firstMerged = firstRound.foldLeft(List[Rectangle]())((acc, rect) => {
      acc.headOption.map(prev => {
        if (prev.left == rect.left && prev.right == rect.right && prev.top == rect.bottom)
          Rectangle(prev.left, prev.bottom, prev.right, rect.top) :: acc.tail
        else
          rect :: acc
      }).getOrElse(List(rect))
    })

    val secondRound = Rectangle.sortByTop(firstMerged)

    secondRound.foldLeft(List[Rectangle]())((acc, rect) => {
      acc.headOption.map(prev => {
        if (prev.top == rect.top && prev.bottom == rect.bottom && prev.right == rect.left)
          Rectangle(prev.left, prev.bottom, rect.right, prev.top) :: acc.tail
        else
          rect :: acc
      }).getOrElse(List(rect))
    })
  }
}

case class Cluster[T](anchor: DataPoint[T], pts: Set[DataPoint[T]]) {
  def bounds = {
    val minL = pts.map(_.lng).min
    val maxR = pts.map(_.lng).max
    val minB = pts.map(_.lat).min
    val maxT = pts.map(_.lat).max
    (DataPoint[Any](minB, minL), DataPoint[Any](maxT, maxR))
  }
}

object Cluster {
  def buildClusters[T](pts: List[DataPoint[T]], clusterRad: Double = 40000.0): Set[Cluster[T]] = {
    val setup: scala.collection.mutable.Set[(DataPoint[T], scala.collection.mutable.Set[DataPoint[T]])] = scala.collection.mutable.Set()
    pts.map(pt=>{
      val closest = setup.map(c => (c, c._1.distanceTo(pt))).filter(_._2 < clusterRad).toList.sortBy(_._2).map(_._1).headOption
      closest.map(p => p._2.add(pt)).getOrElse({
        val s = (pt, scala.collection.mutable.Set(pt))
        setup.add(s)
        s
      })
    })

    setup.map(p=>{
      new Cluster(p._1, p._2.toSet)
    }).toSet
  }

  def buildClusters2[T](pts: List[DataPoint[T]], clusterRad: Double = 40000.0): Set[Cluster[T]] = {
    def recurse(pts: List[DataPoint[T]]): Set[Cluster[T]] = {
      pts.headOption.map(anchor => {
        val in = pts.tail.filter(p=>p.distanceTo(anchor) < clusterRad).toSet
        val out = pts.tail.filterNot(p=>in.contains(p))
        recurse(out) + Cluster(anchor, in)
      }).getOrElse(Set[Cluster[T]]())
    }
    recurse(pts)
  }
}

case class MapGrid(latGridSizeInMeters: Int,
                   latLngRatio: Double,
                   points: Set[DataPoint[VisitData]]) {

  val eqToPoleInMeters = 10000000.0 // Roughly
  val metersInDegLat = eqToPoleInMeters / 90.0 // Roughly
  val gridsInDegLat = metersInDegLat / latGridSizeInMeters

  val latSnap = 1/gridsInDegLat
  val lngSnap = latSnap * latLngRatio
  val latVals = 180 / latSnap
  val lngVals = 360 / lngSnap
  var latOne = 1/latSnap
  val lngOne = 1/lngSnap

  def snapPoint[T](pt: DataPoint[T]): DataPoint[T] = {
    val snapLat = math.floor(pt.lat*latOne)/latOne
    val snapLng = math.floor(pt.lng*lngOne)/lngOne
    new DataPoint(snapLat, snapLng, pt.data)
  }

  def snapPoints[T](pts: Set[DataPoint[T]]): Set[DataPoint[T]] = {
    pts.map(pt=>snapPoint(pt))
  }

  def mapToGrid[T](pts: Set[DataPoint[T]]): Map[(Double, Double), Set[DataPoint[T]]] = {
    pts.map(pt=>{
      val snapped = snapPoint(pt)
      val key = (snapped.lat, snapped.lng)
      (key, pt)
    }).groupBy(_._1).map(p=>(p._1, p._2.map(_._2)))
  }

  protected def combineSnapPoints(pts: Set[DataPoint[VisitData]]): Set[DataPoint[VisitData]] = {
    pts.groupBy(pt=>(pt.lat, pt.lng)).toList.map(p=>{
      val totalVisits = p._2.map(pt=>pt.data.map(_.visits).getOrElse(0)).sum
      val commonName = p._2.toList.flatMap(pt=>pt.data.map(_.name)).groupBy(n=>n).toList.sortBy(-_._2.size).map(_._1).headOption.getOrElse(p._1._1 + ", " + p._1._2)
      val lastVisit = MapGrid.sortPointsByRecent(p._2).headOption.flatMap(pt=>pt.data.map(_.lastVisit)).getOrElse(new DateTime)
      DataPoint(p._1._1, p._1._2, Some(VisitData(totalVisits, commonName, lastVisit)))
    }).toSet
  }

  def pointToRect[T](pt: DataPoint[T]): Rectangle = {
    new Rectangle(pt.lng, pt.lat, pt.lng+lngSnap, pt.lat+latSnap)
  }

  protected def decomposeWorldMap[T](rects: List[Rectangle]): Set[Rectangle] = {
    val global = Rectangle(-180, -90, 180, 90)
    val decomp = Rectangle.decompose(List(global), rects, false)
    Rectangle.merge(decomp).toSet
    // decomp.toSet
  }

  lazy val covered: List[Rectangle] = {
    val snapped = snapPoints(points)
    val combined = combineSnapPoints(snapped)
    val sorted = MapGrid.sortPointsByLatLng(combined)
    sorted.map(pt=>pointToRect(pt))
  }

  lazy val decompose: Set[Rectangle] = {
    decomposeWorldMap(covered)
  }

  def nearby: Set[Rectangle] = {
    decompose
  }
}

object MapGrid {
  def sortPointsByVisits(pts: Set[DataPoint[VisitData]]): List[DataPoint[VisitData]] = pts.toList.sortBy(pt=>pt.data.map(-_.visits).getOrElse(0))
  def sortPointsByRecent(pts: Set[DataPoint[VisitData]]): List[DataPoint[VisitData]] = pts.toList.sortBy(pt=>pt.data.map(-_.lastVisit.getMillis).getOrElse(0L))
  def sortPointsByLatLng[T](pts: Set[DataPoint[T]]): List[DataPoint[T]] = pts.toList.sortBy(p=>(p.lat, p.lng))
}
