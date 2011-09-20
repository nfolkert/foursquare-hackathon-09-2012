package org.nfolkert.fssc

case class Score(visited: Int, grouped: Int) {
  def total = visited + grouped
}

object Game {

  def calculateScore(visitedRects: List[Rectangle], latSize: Double, lngSize: Double): Score = {

    val ptList = visitedRects.map(r=>((r.bottom, r.left), r))
    val ptMap = ptList.toMap

    def basePoints = {
      ptList.size
    }

    def groupPoints = {
      ptList.map(r=>{
        val key = r._1
        val upKey = (key._1+latSize, key._2)
        val rightKey = (key._1, key._2+lngSize)

        (if (ptMap.contains(upKey)) 2 else 0) +
        (if (ptMap.contains(rightKey)) 2 else 0)
      }).sum
    }

    Score(basePoints, groupPoints)
  }
}