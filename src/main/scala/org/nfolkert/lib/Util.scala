package org.nfolkert.lib

import org.joda.time.DateTime
import java.util.Date
import net.liftweb.common.Loggable

object Util extends Loggable {
  // All of these are in seconds
  def nowInSeconds = System.currentTimeMillis / 1000
  def secondsFromDate(date: Date) = date.getTime / 1000
  def secondsFromDate(date: DateTime) = date.getMillis / 1000
  def dateFromSeconds(s: Long) = new DateTime(s * 1000L)

  def duration[T](f: => T): (T, Long) = {
    val start = System.currentTimeMillis
    val ret: T = f
    (ret, System.currentTimeMillis - start)
  }

  def logTime[T](name: String)(f: => T): T = {
    val (ret, time): (T, Long) = duration[T](f)
    logger.info(name + " took " + time + " ms")
    ret
  }
}

object T{def apply[T](name: String)(f: => T): T = Util.logTime(name)(f)}
