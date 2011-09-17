package org.nfolkert.fssc

import org.joda.time.DateTime

case class DataPoint[T](lat: Double, lng: Double, data: Option[T]=None) {
  def distanceTo(pt: DataPoint[_]): Double = {
    val theta = lng - pt.lng
    val distance = math.sin(math.toRadians(lat)) * math.sin(math.toRadians(pt.lat)) +
                   math.cos(math.toRadians(lat)) * math.cos(math.toRadians(pt.lat)) * math.cos(math.toRadians(theta))
    return math.floor(6378100 * math.acos(distance))
  }

}

case class VisitData(visits: Int=0, name: String, lastVisit: DateTime=new DateTime) {}

case class Rectangle(left: Double, bottom: Double, right: Double, top: Double) {
  def isEmpty = right <= left || top <= bottom
  def length = right - left
  def height = top - bottom
  def area = length * height

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
  def buildClusters[T](pts: Set[DataPoint[T]], clusterRad: Double = 40000.0): Set[Cluster[T]] = {
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

  protected def decomposeWorldMap[T](pts: List[DataPoint[T]]): Set[Rectangle] = {
    val global = Rectangle(-180, -90, 180, 90)
    val rects = pts.map(pt=>pointToRect(pt))
    val decomp = Rectangle.decompose(List(global), rects, false)
    Rectangle.merge(decomp).toSet
    // decomp.toSet
  }

  def decompose: Set[Rectangle] = {
    val snapped = snapPoints(points)
    val combined = combineSnapPoints(snapped)
    val sorted = MapGrid.sortPointsByLatLng(combined)
    decomposeWorldMap(sorted)
  }
}

object MapGrid {
  def sortPointsByVisits(pts: Set[DataPoint[VisitData]]): List[DataPoint[VisitData]] = pts.toList.sortBy(pt=>pt.data.map(-_.visits).getOrElse(0))
  def sortPointsByRecent(pts: Set[DataPoint[VisitData]]): List[DataPoint[VisitData]] = pts.toList.sortBy(pt=>pt.data.map(-_.lastVisit.getMillis).getOrElse(0L))
  def sortPointsByLatLng[T](pts: Set[DataPoint[T]]): List[DataPoint[T]] = pts.toList.sortBy(p=>(p.lat, p.lng))
}
