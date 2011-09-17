package org.nfolkert.fssc

import org.junit.Test
import org.specs.SpecsMatchers
import org.joda.time.DateTime

class MapGridTest extends SpecsMatchers {

  @Test
  def testMapGrid() {
    val grid = new MapGrid(400, 1.5, Set(
      DataPoint[VisitData](40.7228981, -73.9948043, Some(VisitData(1))),
      DataPoint[VisitData](40.728709, -73.99479668571429, Some(VisitData(1))),
      DataPoint[VisitData](40.725464, -73.990079, Some(VisitData(1))),
      DataPoint[VisitData](45.516169, -122.67073, Some(VisitData(1))),
      DataPoint[VisitData](40.68592592, -73.97349393333333, Some(VisitData(3))),
      DataPoint[VisitData](40.6706459457292, -73.95802974700928, Some(VisitData(1))),
      DataPoint[VisitData](40.76107323, -73.96382, Some(VisitData(1))),
      DataPoint[VisitData](40.759451, -73.965252, Some(VisitData(1))),
      DataPoint[VisitData](40.73321007823572, -73.98759841918945, Some(VisitData(1)))
    ))

    val clusters = Cluster.buildClusters(grid.points)
    val decomposed = grid.decompose

    println(clusters)
    println(decomposed)
  }

  @Test
  def testRectangles() {

    def R(l:Double, b: Double, r: Double, t: Double) = new Rectangle(l, b, r, t)

    ;{
      val r = R(0, 0, 100, 100)
      r.left must_== 0
      r.right must_== 100
      r.bottom must_== 0
      r.top must_== 100
      r.isEmpty must_== false
      r.length must_== 100
      r.height must_== 100
      r.area must_== 10000
    }

    ;{
      def itest(ra: Rectangle, rb: Rectangle, should: Boolean) {
        ra.intersects(rb) must_== should
        rb.intersects(ra) must_== should
      }

      val r1 = R(0, 0, 10, 10)
      val r2 = R(10, 0, 20, 10)
      val r3 = R(0, 0, 1, 1)
      val r4 = R(9, 9, 10, 10)
      val r5 = R(5, 0, 15, 10)

      itest(r1, r2, false)
      itest(r1, r3, true)
      itest(r1, r4, true)
      itest(r1, r5, true)
      itest(r2, r5, true)

      val r6 = R(0, 10, 30, 20)
      val r7 = R(10, 0, 20, 30)

      itest(r6, r7, true)
    }

    ;{
      def itest(ra: Rectangle, rb: Rectangle, should: Rectangle) {
        ra.intersect(rb) must_== should
        rb.intersect(ra) must_== should
      }

      val r1 = R(0, 0, 10, 10)
      val r2 = R(10, 0, 20, 10)
      val r3 = R(0, 0, 1, 1)
      val r4 = R(9, 9, 10, 10)
      val r5 = R(5, 0, 15, 10)

      itest(r1, r2, R(0, 0, 0, 0))
      itest(r1, r3, R(0, 0, 1, 1))
      itest(r1, r4, R(9, 9, 10, 10))
      itest(r1, r5, R(5, 0, 10, 10))
      itest(r2, r5, R(10, 0, 15, 10))

      val cross1 = R(0, 10, 30, 20)
      val cross2 = R(10, 0, 20, 30)

      itest(cross1, cross2, R(10, 10, 20, 20))
    }

    ;{
      def dtest(r1: Rectangle, r2: Rectangle, expected: Set[Rectangle]) {
        val diffs = r1.difference(r2, false)
        diffs.toSet must_== expected
      }

      val r1 = R(0, 0, 5, 5)
      val r2 = R(0, 4, 1, 5)
      dtest(r1, r2, Set(R(0, 0, 1, 4), R(1, 0, 5, 5)))

      val r3 = R(4, 0, 5, 1)
      dtest(r1, r3, Set(R(0, 0, 4, 5), R(4, 1, 5, 5)))

      val r4 = R(4, 4, 5, 5)
      dtest(r1, r4, Set(R(0, 0, 4, 5), R(4, 0, 5, 4)))

      val r5 = R(2, 2, 3, 3)
      dtest(r1, r5, Set(R(0, 0, 2, 5), R(2, 0, 3, 2), R(2, 3, 3, 5), R(3, 0, 5, 5)))

      val r6 = R(6, 6, 7, 7)
      dtest(r1, r6, Set(R(0, 0, 5, 5)))

      val r7 = R(0, 0, 6, 6)
      dtest(r1, r7, Set())
    }

    ;{
      def dtest(srcs: List[Rectangle], rects: List[Rectangle], expected: Set[Rectangle]) {
        val diffs = Rectangle.decompose(srcs, rects, false).toSet
        diffs must_== expected
      }


      val r1 = R(0, 0, 5, 5)
      val r2 = R(0, 4, 1, 5)
      dtest(List(r1), List(r2), Set(R(1, 0, 5, 5), R(0, 0, 1, 4)))

      val r3 = R(4, 0, 5, 1)
      dtest(List(r1), List(r3), Set(R(0, 0, 4, 5), R(4, 1, 5, 5)))

      val r4 = R(4, 4, 5, 5)
      dtest(List(r1), List(r4), Set(R(0, 0, 4, 5), R(4, 0, 5, 4)))

      val r5 = R(2, 2, 3, 3)
      dtest(List(r1), List(r5), Set(R(0, 0, 2, 5), R(2, 3, 3, 5), R(2, 0, 3, 2), R(3, 0, 5, 5)))

      val r6 = R(6, 6, 7, 7)
      dtest(List(r1), List(r6), Set(R(0, 0, 5, 5)))

      val r7 = R(0, 0, 6, 6)
      dtest(List(r1), List(r7), Set())

      dtest(List(r1), List(r2, r3), Set(R(0, 0, 1, 4), R(1, 0, 4, 5), R(4, 1, 5, 5)))
      dtest(List(r1), List(r2, r4), Set(R(0, 0, 1, 4), R(1, 0, 4, 5), R(4, 0, 5, 4)))
      dtest(List(r1), List(r2, r3, r4), Set(R(0, 0, 1, 4), R(1, 0, 4, 5), R(4, 1, 5, 4)))
      dtest(List(r1), List(r2, r4, r3), Set(R(0, 0, 1, 4), R(1, 0, 4, 5), R(4, 1, 5, 4)))
      dtest(List(r1), List(r2, r7), Set())
      dtest(List(r1), List(r2, r6), Set(R(1, 0, 5, 5), R(0, 0, 1, 4)))
    }

  }
}